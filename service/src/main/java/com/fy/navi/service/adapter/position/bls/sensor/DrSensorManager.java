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
import android.os.Message;
import android.os.SystemClock;

import androidx.annotation.NonNull;

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
import com.fy.navi.service.adapter.position.bls.analysis.AnalysisType;
import com.fy.navi.service.adapter.position.bls.listener.IDrSensorListener;
import com.fy.navi.service.adapter.position.bls.listener.ILossRateAnalysisInterface;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/***Sensor数据（陀螺仪、加速度计）管理类***/
public class DrSensorManager implements SensorEventListener, Handler.Callback {
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
    private long mRawAccTime = 0;//录制

    private float mGyroXValue;
    private float mGyroYValue;
    private float mGyroZValue;
    private long mGyroTime = 0;
    private long mRawGyroTime = 0;

    private long mCarSpeedTime;
    private long mRawCarSpeedTime;
    private float mCarSpeed = 0;
    private Runnable mCustomTimer;
    private final IDrSensorListener mListener;
    private boolean mIsRecordRaw = false;//是否开启DR录制
    private AtomicInteger mIsGyroReady;
    private AtomicInteger mIsAccReady;
    // TODO: 2025/2/24 挡位获取
    private int mGear;
    private MountAngleManager.MountAngleInfo mAngleInfo;

    private boolean mIsEnable = true;
    private final AtomicBoolean mIsStarted;
    private Handler mHandler;
    private Handler mTemperatureHandler;
    private final LocSignData mLocSignData;
    private ScheduledFuture mScheduledFuture;
    private Sensor mAccelerometer;
    private static final int MSG_SEND_TEMPERATURE = 1;
    private SensorDirectChannel mDirectChannel;
    private MemoryFile mMemoryFile;

    public DrSensorManager(Context context, IDrSensorListener listener) {
        mSensorManager = (android.hardware.SensorManager) context.getSystemService(SENSOR_SERVICE);
        mListener = listener;
        mIsGyroReady = new AtomicInteger(0);
        mIsAccReady = new AtomicInteger(0);
        mAngleInfo = MountAngleManager.getInstance().getMountAngleInfo();
        mIsStarted = new AtomicBoolean();
        mHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.SENSOR));
        mTemperatureHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.TEMPERATURE), this);
        mLocSignData = new LocSignData();
        initializeTemperature();
        Logger.i(TAG, "init DrSensorManager " + mAngleInfo);
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

    public synchronized void setEnable(boolean isEnable) {
        Logger.i(TAG, "setEnable old=" + mIsEnable + " new=" + isEnable);
        if (isEnable != mIsEnable) {
            if (isEnable) {
                onStart();
            } else {
                onStop();
            }
        }
        mIsEnable = isEnable;
    }

    public synchronized void init() {
        Logger.i(TAG, " init isEnable=" + mIsEnable);
        if (mIsEnable) {
            onStart();
        }
    }

    private void onStart() {
        Logger.i(TAG, "onStart cur status=" + mIsStarted.get() + " isEnable=" + mIsEnable);
        if (mIsStarted.compareAndSet(false, true)) {
            Logger.i(TAG, "mSensorManager registerListener");
            mSensorManager.registerListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_GYROSCOPE), 1000 * 100, mHandler);//陀螺仪传感器
            mSensorManager.registerListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER), 1000 * 100, mHandler);//加速度传感器
            mTemperatureHandler.sendEmptyMessage(MSG_SEND_TEMPERATURE);
            startTimerTask();
        }
    }

    private void onStop() {
        Logger.i(TAG, "onStop cur status=" + mIsStarted.get() + " isEnable=" + mIsEnable);
        if (mIsStarted.compareAndSet(true, false)) {
            Logger.i(TAG, "real stop ");
            stopTimerTask();
            mSensorManager.unregisterListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_GYROSCOPE));
            mSensorManager.unregisterListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER));
            mTemperatureHandler.removeCallbacksAndMessages(null);
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

    private void startTimerTask() {
        Logger.i(TAG, "startTimerTask");
        stopTimerTask();
        mCustomTimer = new Runnable() {
            @Override
            public void run() {
//                Logger.i(TAG, "DrSensorManager startTimerTask :" + mIsGyroReady.get() + ",mIsAccReady.get()：" + mIsAccReady.get() + ",mCarSpeed " + mCarSpeed);
                if (mIsGyroReady.get() == 1) {
                    setLocGyroInfo(false);
                }
                if (mIsAccReady.get() == 1) {
                    setLocAcce3DInfo(false);
                }
                setLocPulseInfo(false);
            }
        };
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(mCustomTimer, 0, 100, TimeUnit.MILLISECONDS);
    }

    private void stopTimerTask() {
        Logger.i(TAG, "stopTimerTask");
        if (mScheduledFuture != null) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mCustomTimer = null;
        }
    }

    /**
     * 陀螺仪
     * 是否开启录制
     *
     * @param isRaw true：开启  false：未开启
     */
    private void setLocGyroInfo(boolean isRaw) {
        long gyroTime, tickTime;
        if (isRaw) {
            if (mRawGyroTime == 0) {
                mRawGyroTime = SystemClock.elapsedRealtime();
            }
            gyroTime = SystemClock.elapsedRealtime() - mRawGyroTime;
            mRawGyroTime = SystemClock.elapsedRealtime();
            tickTime = mRawGyroTime;
        } else {
            if (mGyroTime == 0) {
                mGyroTime = SystemClock.elapsedRealtime();
            }
            gyroTime = SystemClock.elapsedRealtime() - mGyroTime;
            mGyroTime = SystemClock.elapsedRealtime();
            tickTime = mGyroTime;
        }
        LocGyro sensorData = mLocSignData.gyro;
        sensorData.axis = LocThreeAxis.LocAxisAll; // 有效数据轴
        sensorData.valueX = mGyroXValue;
        sensorData.valueY = mGyroYValue;
        sensorData.valueZ = mGyroZValue;
        sensorData.temperature = mTemperature;
        Logger.d(TAG, mTemperature);
        sensorData.tickTime = BigInteger.valueOf(tickTime);
        sensorData.interval = (int) gyroTime;
        sensorData.dataType = LocDataType.LocDataGyro;
        mListener.onLocGyroInfo(mLocSignData, isRaw);
    }

    /**
     * 3D加速度计
     * 是否开启录制
     *
     * @param isRaw true：开启  false：未开启
     */
    private void setLocAcce3DInfo(boolean isRaw) {
        long accTime, tickTime;
        if (isRaw) {
            if (mRawAccTime == 0) {
                mRawAccTime = SystemClock.elapsedRealtime();
            }
            accTime = SystemClock.elapsedRealtime() - mRawAccTime;
            mRawAccTime = SystemClock.elapsedRealtime();
            tickTime = mRawAccTime;
        } else {
            if (mAccTime == 0) {
                mAccTime = SystemClock.elapsedRealtime();
            }
            accTime = SystemClock.elapsedRealtime() - mAccTime;
            mAccTime = SystemClock.elapsedRealtime();
            tickTime = mAccTime;
        }

        LocAcce3d sensorData = mLocSignData.acce3D;
        sensorData.axis = LocThreeAxis.LocAxisAll; // 有效数据轴
        sensorData.acceX = mAccXValue;
        sensorData.acceY = mAccYValue;
        sensorData.acceZ = mAccZValue;
        sensorData.tickTime = BigInteger.valueOf(tickTime);
        sensorData.interval = (int) accTime;
        sensorData.dataType = LocDataType.LocDataAcce3D;
        mListener.onLocAcce3dInfo(mLocSignData, isRaw);
    }

    /**
     * 速度脉冲
     * 是否开启录制
     *
     * @param isRaw true：开启  false：未开启
     */
    private void setLocPulseInfo(boolean isRaw) {
        long speedTime, tickTime;
        if (isRaw) {
            if (mRawCarSpeedTime == 0) {
                mRawCarSpeedTime = SystemClock.elapsedRealtime();
            }
            speedTime = SystemClock.elapsedRealtime() - mRawCarSpeedTime;
            mRawCarSpeedTime = SystemClock.elapsedRealtime();
            tickTime = mRawCarSpeedTime;
        } else {
            if (mCarSpeedTime == 0) {
                mCarSpeedTime = SystemClock.elapsedRealtime();
            }
            speedTime = SystemClock.elapsedRealtime() - mCarSpeedTime;
            mCarSpeedTime = SystemClock.elapsedRealtime();
            tickTime = mCarSpeedTime;
        }
        if (mCarSpeedTime == 0) {
            mCarSpeedTime = SystemClock.elapsedRealtime();
        }
        LocPulse locPulse = mLocSignData.pulse;
        locPulse.tickTime = BigInteger.valueOf(tickTime);
        locPulse.interval = (int) speedTime;
        locPulse.dataType = LocDataType.LocDataPulse;
        locPulse.value = mCarSpeed;
        mListener.onLocPulseInfo(mLocSignData, isRaw);
    }

    public void setRecordRaw(boolean recordRaw) {
        Logger.i(TAG, " setRecordRaw " + recordRaw);
        mIsRecordRaw = recordRaw;
        mRawAccTime = 0;
        mRawGyroTime = 0;
        mRawCarSpeedTime = 0;
    }

    public void uninit() {
        Logger.i(TAG, " unInit");
        onStop();
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
                        if (mIsRecordRaw) {
                            setLocAcce3DInfo(true);
                        }
                    } else {
                        //陀螺仪
                        mGyroXValue = (float) (x * GYR_UNIT);
                        mGyroYValue = (float) (y * GYR_UNIT);
                        mGyroZValue = (float) (z * GYR_UNIT);
                        if (mIsGyroReady.get() == 0) {
                            mIsGyroReady.compareAndSet(0, 1);
                        }
                        if (mIsRecordRaw) {
                            setLocGyroInfo(true);
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
    public void onSpeedChanged(float speed) {
        //        Logger.d(TAG, "  onVelocityPulseChanged=" + speed);
        int ratio = 1;
        if (mGear == PositionConstant.GearType.GEAR_REVERSE) {
            ratio = -1;
        }
        mCarSpeed = ratio * speed;
        if (mIsRecordRaw) {
            setLocPulseInfo(true);
        }
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

    @Override
    public boolean handleMessage(@NonNull Message msg) {
        if (msg.what == MSG_SEND_TEMPERATURE) {
            parseTemperature();
            mTemperatureHandler.removeMessages(MSG_SEND_TEMPERATURE);
            mTemperatureHandler.sendEmptyMessageDelayed(MSG_SEND_TEMPERATURE, 100);
        }
        return false;
    }

    private void parseTemperature() {
        try {
            if (mMemoryFile != null) {
                byte[] buffer = new byte[1024];
                int bytesRead = mMemoryFile.readBytes(buffer, 0, 0, 1024);
                if (bytesRead >= 4) {
                    // Log raw buffer for debugging
                    StringBuilder hex = new StringBuilder();
                    for (byte b : buffer) {
                        hex.append(String.format("%02X ", b));
                    }
//                    Logger.d(TAG, "Raw buffer: " + hex.toString());
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
