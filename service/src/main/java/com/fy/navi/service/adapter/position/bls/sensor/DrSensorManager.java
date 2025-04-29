package com.fy.navi.service.adapter.position.bls.sensor;

import static android.content.Context.SENSOR_SERVICE;

import android.content.Context;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.os.Handler;
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
import com.fy.navi.service.adapter.position.bls.analysis.AnalysisType;
import com.fy.navi.service.adapter.position.bls.listener.IDrSensorListener;
import com.fy.navi.service.adapter.position.bls.listener.ILossRateAnalysisInterface;

import java.math.BigInteger;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/***Sensor数据（陀螺仪、加速度计）管理类***/
public class DrSensorManager implements SensorEventListener {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private final SensorManager mSensorManager;
    // 加速度计单位（系统给的是m/s^2，需要转换成定位需要的g）
    private static final double ACC_UNIT = 9.81;
    // 陀螺仪单位（系统给的是red/s，需要转成度/s)
    private static final double GYR_UNIT = 180 / 3.1415926;
    private static final long MAX_TIME_MILLS = 20 * 1000;
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
    private long mLastGyrTimeMills;
    private long mLastAccTimeMills;
    private long mLastPluseTimeMills;
    private Handler mHandler;
    private final LocSignData mLocSignData;
    private ScheduledFuture mScheduledFuture;
    private final ILossRateAnalysisInterface mLossRateAnalysisInterface;

    public DrSensorManager(Context context, IDrSensorListener listener, ILossRateAnalysisInterface lossRateAnalysisInterface) {
        mSensorManager = (android.hardware.SensorManager) context.getSystemService(SENSOR_SERVICE);
        mListener = listener;
        mLossRateAnalysisInterface = lossRateAnalysisInterface;
        mIsGyroReady = new AtomicInteger(0);
        mIsAccReady = new AtomicInteger(0);
        mAngleInfo = MountAngleManager.getInstance().getMountAngleInfo();
        Logger.i(TAG, "init DrSensorManager " + mAngleInfo);
        mIsStarted = new AtomicBoolean();
        mHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.SENSOR));
        mLocSignData = new LocSignData();
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
            mSensorManager.registerListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_AMBIENT_TEMPERATURE), 1000 * 100, mHandler);//环境温度传感器
            mLastAccTimeMills = SystemClock.elapsedRealtime();
            mLastGyrTimeMills = SystemClock.elapsedRealtime();
            mLastPluseTimeMills = SystemClock.elapsedRealtime();
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
            mSensorManager.unregisterListener(this, mSensorManager.getDefaultSensor(Sensor.TYPE_AMBIENT_TEMPERATURE));
        }
    }

    private void startTimerTask() {
        Logger.i(TAG, "startTimerTask");
        stopTimerTask();
        mCustomTimer = new Runnable() {
            @Override
            public void run() {
//                Logger.i(TAG, "startTimerTask :" + mIsGyroReady.get() + ",mIsAccReady.get()：" + mIsAccReady.get());
                checkValid();
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

    private void checkValid() {
        if (SystemClock.elapsedRealtime() - mLastAccTimeMills > MAX_TIME_MILLS) {
            StringBuilder builder = new StringBuilder();
            builder.append(System.currentTimeMillis());
            builder.append(";onSensorError no acc data more than mills=");
            builder.append(SystemClock.elapsedRealtime() - mLastAccTimeMills);
            Logger.e(TAG, builder.toString());
            mListener.onSensorError(builder.toString());
            mLastAccTimeMills = SystemClock.elapsedRealtime();
        }
        if (SystemClock.elapsedRealtime() - mLastGyrTimeMills > MAX_TIME_MILLS) {
            StringBuilder builder = new StringBuilder();
            builder.append(System.currentTimeMillis());
            builder.append(";onSensorError no gyr data more than mills=");
            builder.append(SystemClock.elapsedRealtime() - mLastGyrTimeMills);
            Logger.e(TAG, builder.toString());
            mListener.onSensorError(builder.toString());
            mLastGyrTimeMills = SystemClock.elapsedRealtime();
        }
        if (SystemClock.elapsedRealtime() - mLastPluseTimeMills > MAX_TIME_MILLS) {
            StringBuilder builder = new StringBuilder();
            builder.append(System.currentTimeMillis());
            builder.append(";onSensorError no pluse data more than mills=");
            builder.append(SystemClock.elapsedRealtime() - mLastPluseTimeMills);
            builder.append("  speed=");
            builder.append(mCarSpeed);
            Logger.e(TAG, builder.toString());
            mListener.onSensorError(builder.toString());
            mLastPluseTimeMills = SystemClock.elapsedRealtime();
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
        Logger.d("mTemperature",mTemperature);
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
        Logger.i(TAG, " mCarSpeed " + mCarSpeed);
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
                        mLossRateAnalysisInterface.analysis(AnalysisType.ACC);
                        mLastAccTimeMills = SystemClock.elapsedRealtime();
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
                        mLossRateAnalysisInterface.analysis(AnalysisType.GYR);
                        mLastGyrTimeMills = SystemClock.elapsedRealtime();
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

        } else if (sensorType == Sensor.TYPE_AMBIENT_TEMPERATURE) {
            float[] values = event.values;
            if (values != null && values.length >= 1) {
                mTemperature = values[0];
            }
        }
    }

    /**
     * 车速变化
     */
    public void onSpeedChanged(float speed) {
        //        Logger.d(TAG, "  onVelocityPulseChanged=" + speed);
        mLastPluseTimeMills = SystemClock.elapsedRealtime();
        int ratio = 1;
        if (mGear == PositionConstant.GearType.GEAR_REVERSE) {
            ratio = -1;
        }
        mCarSpeed = ratio * speed;
        mLossRateAnalysisInterface.analysis(AnalysisType.PLUSE);
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
}
