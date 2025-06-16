package com.fy.navi.service.adapter.position.bls.sensor;

import static android.content.Context.SENSOR_SERVICE;

import android.content.Context;
import android.hardware.Sensor;
import android.hardware.SensorDirectChannel;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.os.Handler;
import android.os.MemoryFile;
import android.os.SystemClock;

import com.android.utils.log.Logger;
import com.android.utils.thread.LooperType;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.pos.model.LocAcce3d;
import com.autonavi.gbl.pos.model.LocDataType;
import com.autonavi.gbl.pos.model.LocGyro;
import com.autonavi.gbl.pos.model.LocPulse;
import com.autonavi.gbl.pos.model.LocSignData;
import com.autonavi.gbl.pos.model.LocThreeAxis;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.PositionConstant;
import com.fy.navi.service.adapter.position.bls.listener.IDrSensorListener;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/***Sensor数据（陀螺仪、加速度计、温度、脉冲）管理类***/
public class DrSensorManager implements SensorEventListener {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private final SensorManager mSensorManager;
    // 加速度计单位（系统给的是m/s^2，需要转换成定位需要的g）
    private static final double ACC_UNIT = 9.81;
    // 陀螺仪单位（系统给的是red/s，需要转成度/s)
    private static final double GYR_UNIT = 180 / 3.1415926;
    private boolean mIsSupported;
    private float mTemperature = 0;
    private float mAccXValue;
    private float mAccYValue;
    private float mAccZValue;
    private long mAccTime = 0;

    private float mGyroXValue;
    private float mGyroYValue;
    private float mGyroZValue;
    private long mGyroTime = 0;

    private long mCarSpeedTime;
    private float mCarSpeed = 0;
    private Runnable mCustomTimer;
    private final IDrSensorListener mListener;
    private AtomicInteger mIsGyroReady = new AtomicInteger(0);
    private AtomicInteger mIsAccReady = new AtomicInteger(0);
    private AtomicInteger mTemperatureCount = new AtomicInteger(0);
    // TODO: 2025/2/24 挡位获取
    private int mGear;
    private MountAngleManager.MountAngleInfo mAngleInfo;
    private AtomicBoolean mIsStarted = new AtomicBoolean();
    private Handler mHandler;
    private final LocSignData mLocSignData;
    private ScheduledFuture mScheduledFuture;
    private Sensor mAccelerometer;
    private SensorDirectChannel mDirectChannel;
    private MemoryFile mMemoryFile;

    public DrSensorManager(Context context, IDrSensorListener listener) {
        Logger.i(TAG, "DrSensorManager ");
        mSensorManager = (android.hardware.SensorManager) context.getSystemService(SENSOR_SERVICE);
        mListener = listener;
        mLocSignData = new LocSignData();
        mAngleInfo = MountAngleManager.getInstance().getMountAngleInfo();
    }

    public synchronized void init() {
        Logger.i(TAG, "init status=" + mIsStarted.get());
        if (mIsStarted.compareAndSet(false, true)) {
            Logger.i(TAG, "real init");
            mHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.SENSOR));
            initializeTemperature();
            mSensorManager.registerListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_GYROSCOPE), 1000 * 100, mHandler);//陀螺仪传感器
            mSensorManager.registerListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER), 1000 * 100, mHandler);//加速度传感器
            mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(this::startSensorReport, 0, 100, TimeUnit.MILLISECONDS);
        }
    }

    public void uninit() {
        Logger.i(TAG, "unInit cur status=" + mIsStarted.get());
        if (mIsStarted.compareAndSet(true, false)) {
            Logger.i(TAG, "real stop ");
            stopSensorReport();
            mSensorManager.unregisterListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_GYROSCOPE));
            mSensorManager.unregisterListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER));
            if (mDirectChannel != null) {
                mDirectChannel.close();
            }
            if (mMemoryFile != null) {
                mMemoryFile.close();
            }
            mMemoryFile = null;
            mDirectChannel = null;
        }
    }

    private void initializeTemperature() {
        try {
            mAccelerometer = mSensorManager.getDefaultSensor(Sensor.TYPE_DEVICE_PRIVATE_BASE);
            mMemoryFile = new MemoryFile("GyroTemp", 1024);
            if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
                mIsSupported = mAccelerometer.isDirectChannelTypeSupported(
                        SensorDirectChannel.TYPE_HARDWARE_BUFFER);
                mDirectChannel = mSensorManager.createDirectChannel(mMemoryFile);
                if (mDirectChannel != null) {
                    mDirectChannel.configure(mAccelerometer, SensorDirectChannel.RATE_NORMAL);
                }
            }
            Logger.i(TAG, ",mIsSupported：" + mIsSupported);
        } catch (Exception e) {
            Logger.e(TAG, "Exception：" + e.toString());
        }
    }

    private void startSensorReport() {
        Logger.i(TAG, " startTimerTask :" + mIsGyroReady.get() + ",mIsAccReady.get()：" + mIsAccReady.get() + ",mCarSpeed " + mCarSpeed + ",mTemperatureCount：" + mTemperatureCount.get());
        if (mIsGyroReady.get() == 1) {
            setLocGyroInfo();
        }
        if (mIsAccReady.get() == 1) {
            setLocAcce3DInfo();
        }
        setLocPulseInfo();
        if (mTemperatureCount.get() == 0) {
            parseTemperature();
        } else {
            mTemperatureCount.incrementAndGet();
            if (mTemperatureCount.get() == 10) {
                mTemperatureCount.set(0);
            }
        }
    }

    private void stopSensorReport() {
        Logger.i(TAG, "stopTimerTask");
        if (mScheduledFuture != null) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mCustomTimer = null;
        }
    }

    /**
     * 陀螺仪
     */
    private void setLocGyroInfo() {
        long gyroTime, tickTime;
        if (mGyroTime == 0) {
            mGyroTime = SystemClock.elapsedRealtime();
        }
        gyroTime = SystemClock.elapsedRealtime() - mGyroTime;
        mGyroTime = SystemClock.elapsedRealtime();
        tickTime = mGyroTime;
        LocGyro sensorData = mLocSignData.gyro;
        sensorData.axis = LocThreeAxis.LocAxisAll; // 有效数据轴
        sensorData.valueX = mGyroXValue;
        sensorData.valueY = mGyroYValue;
        sensorData.valueZ = mGyroZValue;
        sensorData.temperature = mTemperature;
        sensorData.tickTime = BigInteger.valueOf(tickTime);
        sensorData.interval = (int) gyroTime;
        sensorData.dataType = LocDataType.LocDataGyro;
        mListener.onLocGyroInfo(mLocSignData);
    }

    /**
     * 3D加速度计
     */
    private void setLocAcce3DInfo() {
        long accTime, tickTime;
        if (mAccTime == 0) {
            mAccTime = SystemClock.elapsedRealtime();
        }
        accTime = SystemClock.elapsedRealtime() - mAccTime;
        mAccTime = SystemClock.elapsedRealtime();
        tickTime = mAccTime;
        LocAcce3d sensorData = mLocSignData.acce3D;
        sensorData.axis = LocThreeAxis.LocAxisAll; // 有效数据轴
        sensorData.acceX = mAccXValue;
        sensorData.acceY = mAccYValue;
        sensorData.acceZ = mAccZValue;
        sensorData.tickTime = BigInteger.valueOf(tickTime);
        sensorData.interval = (int) accTime;
        sensorData.dataType = LocDataType.LocDataAcce3D;
        mListener.onLocAcce3dInfo(mLocSignData);
    }

    /**
     * 速度脉冲
     */
    private void setLocPulseInfo() {
        long speedTime, tickTime;
        if (mCarSpeedTime == 0) {
            mCarSpeedTime = SystemClock.elapsedRealtime();
        }
        speedTime = SystemClock.elapsedRealtime() - mCarSpeedTime;
        mCarSpeedTime = SystemClock.elapsedRealtime();
        tickTime = mCarSpeedTime;
        LocPulse locPulse = mLocSignData.pulse;
        locPulse.tickTime = BigInteger.valueOf(tickTime);
        locPulse.interval = (int) speedTime;
        locPulse.dataType = LocDataType.LocDataPulse;
        locPulse.value = mCarSpeed;
        mListener.onLocPulseInfo(mLocSignData);
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        handleGyroAndAccData(event);
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {

    }

    private int valuesLength;

    private void handleGyroAndAccData(final SensorEvent event) {
        if (event == null) {
            Logger.e(TAG, "handleGyroAndAccData: sensorEvent=null");
            return;
        }
        int sensorType = event.sensor.getType();
//        Logger.i(TAG, "sensorType：" + sensorType);
        if (sensorType == Sensor.TYPE_ACCELEROMETER || sensorType == Sensor.TYPE_GYROSCOPE) {
            float[] values = event.values;
            if (values != null) {
                if (valuesLength != values.length) {
                    valuesLength = values.length;
                    Logger.i(TAG, "values.length：" + valuesLength + ",values：" + values[0] + "," + values[1] + "," + values[2]);
                }
                if (values.length >= 3) {
                    float x = values[mAngleInfo.x];
                    float y = values[mAngleInfo.y];
                    float z = values[mAngleInfo.z];

                    if (sensorType == Sensor.TYPE_ACCELEROMETER) {
                        //加速度
                        mAccXValue = (float) (x / ACC_UNIT);
                        mAccYValue = (float) (y / ACC_UNIT);
                        mAccZValue = (float) (z / ACC_UNIT);
                        if (mIsAccReady.get() == 0) {
                            mIsAccReady.compareAndSet(0, 1);
                        }
                    } else {
                        //陀螺仪
                        mGyroXValue = (float) (x * GYR_UNIT);
                        mGyroYValue = (float) (y * GYR_UNIT);
                        mGyroZValue = (float) (z * GYR_UNIT);
                        if (mIsGyroReady.get() == 0) {
                            mIsGyroReady.compareAndSet(0, 1);
                        }
                    }
                }
            } else {
                Logger.e(TAG, "values == null");
            }
        }
    }

    /**
     * 车速变化
     */
    public void onPulseSpeedChanged(float speed) {
        int ratio = 1;
        if (mGear == PositionConstant.GearType.GEAR_REVERSE) {
            ratio = -1;
        }
        mCarSpeed = ratio * speed;
    }

    /**
     * GEAR_NEUTRAL = 1;空挡
     * GEAR_REVERSE = 2;倒挡
     * GEAR_PARK = 4;停车挡
     * GEAR_DRIVE = 8;行驶挡
     * GEAR_LOW = 8192;低速挡
     * GEAR_MANUAL = 16384;手动挡
     *
     * @param gear 挡位
     */
    public void onGearChanged(int gear) {
        mGear = gear;
        Logger.d(TAG, " onGearChanged gear=" + gear);
    }

    private void parseTemperature() {
        try {
            if (mMemoryFile != null) {
                byte[] buffer = new byte[1024];
                int bytesRead = mMemoryFile.readBytes(buffer, 0, 0, 1024);
                if (bytesRead >= 4) {
                    ByteBuffer byteBuffer = ByteBuffer.wrap(buffer).order(ByteOrder.LITTLE_ENDIAN);
                    mTemperature = byteBuffer.getFloat(24); // Adjust offset if needed
                    Logger.d(TAG, "Parsed temperature: " + mTemperature + "°C");
                } else {
                    Logger.d(TAG, "Waiting for data...");
                }
            }
        } catch (Exception e) {
            Logger.e(TAG, "Failed to parse temperature: " + e.getMessage());
        }
    }
}
