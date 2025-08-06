package com.sgm.navi.service.adapter.signal.bls;

import android.car.Car;
import android.car.hardware.CarPropertyValue;
import android.car.hardware.property.CarPropertyManager;
import android.car.media.CarAudioManager;
import android.content.Context;
import android.hardware.automotive.vehicle.V2_0.VehicleArea;
import android.os.Handler;
import android.os.HandlerThread;

import com.android.utils.log.Logger;
import com.patac.vehicle.DriveAssistController;
import com.patac.vehicle.HvacController;
import com.patac.vehicle.PowertainController;
import com.patac.vehicle.VehicleController;
import com.patac.vehicle.VehicleStatusController;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.signal.SignalAdapterCallback;
import com.sgm.navi.service.adapter.signal.SignalApi;
import com.sgm.navi.service.define.signal.RoadConditionGroup;
import com.sgm.navi.service.define.signal.SdNavigationStatusGroup;
import com.sgm.navi.service.define.signal.SignalConst;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

import gm.powermode.PowerModeManager;
import vendor.gm.vehicle.V1_0.VendorProperty;
import vendor.patac.vehicle.V1_0.PatacProperty;

public class SignalAdapterImpl implements SignalApi {
    private static final String TAG = MapDefaultFinalTag.SIGNAL_SERVICE_TAG;

    private HandlerThread mWorkThread;
    private VehicleController mVehicleController;
    private final List<SignalAdapterCallback> mCallbacks = new CopyOnWriteArrayList<>();
    private Car mCar;
    private CarPropertyManager mPropertyManager;
    private CarAudioManager mCarAudioManager;

    private float maxBatteryEnergy = -1;
    private float mBatteryEnergy = -1;
    private int mChargeSystemStatus = -1;
    private float mOutsideTemperature = -1;
    private float mHighVoltageBatteryPropulsionRange = -1;
    private float mRangeRemaining = -1;
    private int mBatteryEnergyPercent = -1;
    private int mPredictedFuelSavingPer100km = -1;
    private int mTotalFuelSaving = -1;


    public SignalAdapterImpl() {
    }

    /**
     * 初始化回调函数
     */
    private void initCallback() {
        PowertainController.getInstance().registerHighVoltageChargeSystemStatusListener(new PowertainController.HighVoltageChargeSystemStatusListener() {
            @Override
            public void onHighVoltageChargeSystemStatusChanged(Integer value) {
                Logger.d(TAG, "onHighVoltageChargeSystemStatusChanged: ", value);
                mChargeSystemStatus = value;
            }
        });
        VehicleStatusController.getInstance().registerOutsideTemperatureListener(new VehicleStatusController.OutsideTemperatureListener() {
            @Override
            public void onOutsideTemperatureSignalChanged(Float value) {
                // 1hz
                Logger.d(TAG, "onOutsideTemperatureSignalChanged: ", value);
                mOutsideTemperature = value;
            }
        });
        PowertainController.getInstance().registerFuelLevelPercentListener(new PowertainController.FuelLevelPercentListener() {
            @Override
            public void onFuelLevelPercentSignalChanged(Float value) {
                Logger.d(TAG, "onFuelLevelPercentSignalChanged: " + value);
                for (SignalAdapterCallback callback : mCallbacks) {
                    callback.onFuelLevelPercentSignalChanged(value);
                }
            }
        });
        PowertainController.getInstance().registerVehicleMotionMovementStateListener(new PowertainController.VehicleMotionMovementStateListener() {
            @Override
            public void onVehicleMotionMovementStateChanged(final Integer gear) {
                Logger.d(TAG, "onVehicleMotionMovementStateChanged: " + gear);
                for (SignalAdapterCallback callback : mCallbacks) {
                    callback.onGearChanged(gear);
                }
            }
        });
        PowertainController.getInstance().registerHighVoltageBatteryPropulsionRangeListener(
                new PowertainController.HighVoltageBatteryPropulsionRangeListener() {
                    @Override
                    public void onHighVoltageBatteryPropulsionRangeChanged(final float value) {
                        Logger.d(TAG, "onHighVoltageBatteryPropulsionRangeChanged: " + value);
                        mHighVoltageBatteryPropulsionRange = value;
                        for (SignalAdapterCallback callback : mCallbacks) {
                            callback.onHighVoltageBatteryPropulsionRangeChanged(value);
                        }
                    }
                });
        PowerModeManager.getInstance().registerSystemStateListener((newMode, oldMode) -> {
            Logger.d(TAG, "onSystemStateChanged: oldMode = " + oldMode + ", newMode = " + newMode);
            for (SignalAdapterCallback callback : mCallbacks) {
                callback.onSystemStateChanged(systemStateConversion(newMode));
            }
            return 0;
        });
        PowertainController.getInstance().registerHighVoltageBatteryRemainingUsableEnergyListener(new PowertainController.HighVoltageBatteryRemainingUsableEnergyListener() {
            @Override
            public void onHighVoltageBatteryRemainingUsableEnergySignalChanged(Float value) {
                Logger.d(TAG, "onHighVoltageBatteryRemainingUsableEnergySignalChanged: ", value);
                mBatteryEnergy = value;
            }
        });
        if (VehicleController.isGBArch()) {
            PowertainController.getInstance().registerInfoEvBatteryCapacityListener(new PowertainController.InfoEvBatteryCapacityListener() {
                @Override
                public void onInfoEvBatteryCapacityChanged(Float value) {
                    Logger.d(TAG, "onInfoEvBatteryCapacityChanged: ", value);
                    maxBatteryEnergy = value;
                }
            });
            PowertainController.getInstance().registerRangeRemainingListener(new PowertainController.RangeRemainingListener() {
                @Override
                public void onRangeRemainingSignalChanged(final Float value) {
                    Logger.d(TAG, "onRangeRemainingSignalChanged: " + value);
                    mRangeRemaining = value;
                    for (SignalAdapterCallback callback : mCallbacks) {
                        callback.onRangeRemainingSignalChanged(value);
                    }
                }
            });
        }
        if (!VehicleController.isCleaArch()) {
            Logger.i(TAG, "is not CleaArch");
            return;
        }
        DriveAssistController.getInstance().registerNaviOnAdasCloseToNoaAreaListener(
                new DriveAssistController.NaviOnAdasCloseToNoaAreaListener() {
                    @Override
                    public void onNaviOnAdasCloseToNoaAreaChanged(Boolean aBoolean) {
                        Logger.d(TAG, aBoolean);
                        if (aBoolean) {
                            for (SignalAdapterCallback callback : mCallbacks) {
                                callback.onNaviOnADASStateChanged(SignalConst.L2_NOP.CLOSE_TO_NOA_AREA_TRUE);
                            }
                        }
                    }
                });
        DriveAssistController.getInstance().registerNaviOnAdasMergeIntoMainRoadListener(
                new DriveAssistController.NaviOnAdasMergeIntoMainRoadListener() {
                    @Override
                    public void onNaviOnAdasMergeIntoMainRoadChanged(Boolean aBoolean) {
                        Logger.d(TAG, aBoolean);
                        if (aBoolean) {
                            for (SignalAdapterCallback callback : mCallbacks) {
                                callback.onNaviOnADASStateChanged(SignalConst.L2_NOP.MERGE_INTO_MAIN_ROAD_TRUE);
                            }
                        }
                    }
                });
        DriveAssistController.getInstance().registerNaviOnAdasLaneChangingToFollowRouteListener(
                new DriveAssistController.NaviOnAdasLaneChangingToFollowRouteListener() {
                    @Override
                    public void onNaviOnAdasLaneChangingToFollowRouteChanged(int value) {
                        Logger.d(TAG, value);
                        if (value == 1) {
                            value = SignalConst.L2_NOP.LANE_CHANGING_TO_FOLLOW_ROUTE_LEFT;
                        } else if (value == 2) {
                            value = SignalConst.L2_NOP.LANE_CHANGING_TO_FOLLOW_ROUTE_RIGHT;
                        }
                        for (SignalAdapterCallback callback : mCallbacks) {
                            callback.onNaviOnADASStateChanged(value);
                        }
                    }
                });
        DriveAssistController.getInstance().registerNaviOnAdasTextToSpeechLaneChangeAbortListener(
                new DriveAssistController.NaviOnAdasTextToSpeechLaneChangeAbortListener() {
                    @Override
                    public void onNaviOnAdasTextToSpeechLaneChangeAbortChanged(Boolean aBoolean) {
                        Logger.d(TAG, aBoolean);
                        if (aBoolean) {
                            for (SignalAdapterCallback callback : mCallbacks) {
                                callback.onNaviOnADASStateChanged(SignalConst.L2_NOP.TEXT_TO_SPEECH_LANE_CHANGE_ABORT_TRUE);
                            }
                        }
                    }
                });
        DriveAssistController.getInstance().registerNaviOnAdasChangingToFastLaneListener(
                new DriveAssistController.NaviOnAdasChangingToFastLaneListener() {
                    @Override
                    public void onNaviOnAdasChangingToFastLaneChanged(int value) {
                        Logger.d(TAG, value);
                        if (value == 1) {
                            value = SignalConst.L2_NOP.CHANGING_TO_FAST_LANE_LEFT;
                        } else if (value == 2) {
                            value = SignalConst.L2_NOP.CHANGING_TO_FAST_LANE_RIGHT;
                        }
                        for (SignalAdapterCallback callback : mCallbacks) {
                            callback.onNaviOnADASStateChanged(value);
                        }
                    }
                });
        DriveAssistController.getInstance().registerNaviOnAdasConfirmChangeToFastLaneRequestListener(
                new DriveAssistController.NaviOnAdasConfirmChangeToFastLaneRequestListener() {
                    @Override
                    public void onNaviOnAdasConfirmChangeToFastLaneRequestChanged(int value) {
                        Logger.d(TAG, value);
                        if (value == 1) {
                            value = SignalConst.L2_NOP.CONFIRM_CHANGE_TO_FAST_LANE_LEFT;
                        } else if (value == 2) {
                            value = SignalConst.L2_NOP.CONFIRM_CHANGE_TO_FAST_LANE_RIGHT;
                        }
                        for (SignalAdapterCallback callback : mCallbacks) {
                            callback.onNaviOnADASStateChanged(value);
                        }
                    }
                });
        DriveAssistController.getInstance().registerNaviOnAdasTextToSpeechLaneChangeAbortListener(
                new DriveAssistController.NaviOnAdasTextToSpeechLaneChangeAbortListener() {
                    @Override
                    public void onNaviOnAdasTextToSpeechLaneChangeAbortChanged(Boolean aBoolean) {
                        Logger.d(TAG, aBoolean);
                        if (aBoolean) {
                            for (SignalAdapterCallback callback : mCallbacks) {
                                callback.onNaviOnADASStateChanged(SignalConst.L2_NOP.TEXT_TO_SPEECH_LANE_CHANGE_ABORT_TRUE);
                            }
                        }
                    }
                });
        DriveAssistController.getInstance().registerNaviOnAdasExitRampToNonLimitedAccessRoadListener(
                new DriveAssistController.NaviOnAdasExitRampToNonLimitedAccessRoadListener() {
                    @Override
                    public void onNaviOnAdasExitRampToNonLimitedAccessRoadChanged(Boolean aBoolean) {
                        Logger.d(TAG, aBoolean);
                        if (aBoolean) {
                            for (SignalAdapterCallback callback : mCallbacks) {
                                callback.onNaviOnADASStateChanged(SignalConst.L2_NOP.EXIT_RAMP_TO_NON_LIMITED_ACCESS_ROAD_TRUE);
                            }
                        }
                    }
                });
    }

    private Car.CarServiceLifecycleListener mCarServiceLifecycleListener = new Car.CarServiceLifecycleListener() {
        public void onLifecycleChanged(final Car car, final boolean carReady) {
            Logger.d(TAG, "CarServiceLifecycleListener onLifecycleChanged: " + carReady);
            if (!carReady) {
                reset();
            } else {
                initPropertyManager(car);
            }
        }
    };

    @Override
    public void initSignal(final Context context) {
        Logger.d(TAG, "initSignal start");
        long start = System.currentTimeMillis();
        mWorkThread = new HandlerThread("can-thread");
        mWorkThread.start();

        final Handler handler = new Handler(mWorkThread.getLooper());

        /**
         * 首先调用VehicleController.getInstance获得VehicleController实例.
         * 此class对象负责和CarService建议连接，重连等工作.
         * 在初始化VehicleController的时候，如果传递了具体的Handler，后续的Event处理将会在此Handler所绑定的Looper中处理；
         * 如果传递的Handler是null，后续的Event处理将会主线程Looper中处理 */
        mVehicleController = VehicleController.getInstance(context, handler);
        initCallback();

        mCar = Car.createCar(context, handler, 5000L, this.mCarServiceLifecycleListener);
        if (mCar != null && mCar.isConnected()) {
            Logger.d(TAG, "Car connect successfully in VehicleService constructor");
            initPropertyManager(mCar);
        }
        Logger.d(TAG, "initSignal end", (System.currentTimeMillis() - start)); // 100ms
    }

    private void initMaxBatteryEnergy() {
        if (VehicleController.isCleaArch()) {
            return;
        }
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getInfoEvBatteryCapacity();
            maxBatteryEnergy = result.getValue(-1f);
        } catch (Exception e) {
            Logger.w(TAG, "initMaxBatteryEnergy: " + e.getMessage());
            maxBatteryEnergy = -1;
        }
    }

    private void initBatteryEnergy() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryRemainingUsableEnergy();
            mBatteryEnergy = result.getValue(-1f);
        } catch (Exception e) {
            Logger.e(TAG, "initBatteryEnergy: " + e.getMessage());
            mBatteryEnergy = -1;
        }
    }

    private void initChargeSystemStatus() {
        final VehicleController.Result<Integer> result;
        try {
            result = PowertainController.getInstance().getHighVoltageChargeSystemStatus();
            mChargeSystemStatus = result.getValue(-1);
        } catch (Exception e) {
            Logger.e(TAG, "initChargeSystemStatus: " + e.getMessage());
            mChargeSystemStatus = -1;
        }
    }

    private void initOutsideTemperature() {
        final VehicleController.Result<Float> result;
        try {
            result = VehicleStatusController.getInstance().getOutsideTemperature();
            mOutsideTemperature = result.getValue(-1f);
        } catch (Exception e) {
            Logger.e(TAG, "initOutsideTemperature: " + e.getMessage());
            mOutsideTemperature = -1;
        }
    }

    private void initHighVoltageBatteryPropulsionRange() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryPropulsionRange();
            mHighVoltageBatteryPropulsionRange = result.getValue(-1f);
        } catch (Exception e) {
            Logger.e(TAG, "getHighVoltageBatteryPropulsionRange: " + e.getMessage());
            mHighVoltageBatteryPropulsionRange = -1;
        }
    }

    private void initRangeRemaining() {
        if (VehicleController.isCleaArch()) {
            return;
        }
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getRangeRemaining();
            mRangeRemaining = result.getValue(-1f);
        } catch (Exception e) {
            Logger.e(TAG, "getRangeRemaining: " + e.getMessage());
            mRangeRemaining = -1;
        }
    }

    /**
     * 初始化CarPropertyManager
     *
     * @param car
     */
    private synchronized void initPropertyManager(final Car car) {
        this.mCar = car;
        if (this.mPropertyManager == null) {
            Logger.d(TAG, "init CarPropertyManager");
            this.mPropertyManager = (CarPropertyManager) this.mCar.getCarManager("property");
            initVolumeCallback(car);
            initMaxBatteryEnergy();
            initBatteryEnergy();
            initChargeSystemStatus();
            initOutsideTemperature();
            initHighVoltageBatteryPropulsionRange();
            initRangeRemaining();
            try {
                registerDYN(190, 1, value -> { // 仪表车速
                    for (SignalAdapterCallback callback : mCallbacks) {
                        callback.onSpeedChanged((float) value / 1024);
                    }
                });
                if (VehicleController.isCleaArch()) {
                    registerDYN(21, 3, value -> { // 燃油续航里程
                        Logger.d(TAG, "fuel range: ", value);
                        mRangeRemaining = value;
                    });
                    registerDYN(21, 55, value -> { // 电池剩余电量百分比
                        Logger.d(TAG, "percentage of battery remaining: ", value);
                        mBatteryEnergyPercent = value;
                    });
                }
            } catch (Exception e) {
                Logger.e(TAG, "registerDYN: ", e);
            }
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_STATUS_ACTIVE_INDICATION_ON,
                    SignalConst.L2_NOP.STATUS_ACTIVE_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_STATUS_NORMAL_TO_OVERRIDE_INDICATION_ON,
                    SignalConst.L2_NOP.STATUS_NORMAL_TO_OVERRIDE_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_STATUS_OVERRIDE_TO_NORMAL_INDICATION_ON,
                    SignalConst.L2_NOP.STATUS_OVERRIDE_TO_NORMAL_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_CLOSE_TO_TIGHT_CURVE_INDICATION_ON,
                    SignalConst.L2_NOP.CLOSE_TO_TIGHT_CURVE_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_INTO_TIGHT_CURVE_INDICATION_ON,
                    SignalConst.L2_NOP.INTO_TIGHT_CURVE_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_TAKE_STEERING_INDICATION_ON,
                    SignalConst.L2_NOP.TAKE_STEERING_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_DISTANCE_TO_RAMP_2000_M_INDICATION_ON,
                    SignalConst.L2_NOP.DISTANCE_TO_RAMP_2000M_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_DISTANCE_TO_RAMP_500_M_INDICATION_ON,
                    SignalConst.L2_NOP.DISTANCE_TO_RAMP_500M_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_COMPLICATED_ROAD_CONDITION_LANE_CHANGE_FAILED,
                    SignalConst.L2_NOP.COMPLICATED_ROAD_CONDITION_LANE_CHANGE_FAILED_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_DISTANCE_TO_END_500_M_INDICATION_ON,
                    SignalConst.L2_NOP.DISTANCE_TO_END_500M_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_FINISHED_INDICATION_ON,
                    SignalConst.L2_NOP.FINISHED_INDICATION_ON_TRUE);
            patacPropertyRegister(PatacProperty.NAVIGATION_ON_ADAS_TAKE_VEHICLE_CONTROL_INDICATION_ON,
                    SignalConst.L2_NOP.TAKE_VEHICLE_CONTROL_INDICATION_ON_TRUE);
            mPropertyManager.registerCallback(new CarPropertyManager.CarPropertyEventCallback() {
                @Override
                public void onChangeEvent(CarPropertyValue carPropertyValue) {
                    if (carPropertyValue == null) {
                        Logger.i(TAG, "carPropertyValue == null");
                        return;
                    }
                    Integer value = (Integer) carPropertyValue.getValue();
                    Logger.d(TAG, "value", value);
                    int ttsValue = -1;
                    switch (value) {
                        case 0x3:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_CONSTRUCTION;
                            break;
                        case 0x4:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_MAP_UNAVAILABLE;
                            break;
                        case 0x5:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_GPS_UNAVAILABLE;
                            break;
                        case 0x6:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_TRAFFIC_JAM;
                            break;
                        case 0x9:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_TIGHTCURVE;
                            break;
                        case 0xA:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_SPEEDOUTLIMIT;
                            break;
                        case 0xC:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_COMPLICATED_ROAD_CONDITION;
                            break;
                        case 0xE:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_UNAVAILABLE;
                            break;
                        case 0xF:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_TUNNEL;
                            break;
                        case 0x13:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_SERVICE_NAVIGATION_ON_ADAS_SYSTEM;
                            break;
                        case 0x12:
                            ttsValue = SignalConst.L2_NOP.DEACTIVATION_REASON_DRIVER_ACTION;
                            break;
                    }
                    if (ttsValue != -1) {
                        for (SignalAdapterCallback callback : mCallbacks) {
                            callback.onNaviOnADASStateChanged(ttsValue);
                        }
                    }
                }

                @Override
                public void onErrorEvent(int i, int i1) {
                    Logger.i(TAG, i, i1);
                }
            }, PatacProperty.NAVIGATION_ON_ADAS_DEACTIVATION_REASON, 0);
            mPropertyManager.registerCallback(new CarPropertyManager.CarPropertyEventCallback() {
                @Override
                public void onChangeEvent(CarPropertyValue carPropertyValue) {
                    if (carPropertyValue == null) {
                        Logger.i(TAG, "LANE_CENTERING_WARNING_EXTENDED_INDICATION_REQUEST", "carPropertyValue == null");
                        return;
                    }
                    Integer value = (Integer) carPropertyValue.getValue();
                    Logger.i(TAG, "LANE_CENTERING_WARNING_EXTENDED_INDICATION_REQUEST", value);
                    for (SignalAdapterCallback callback : mCallbacks) {
                        callback.onLaneCenteringWarningIndicationRequestIdcmAChanged(value);
                    }
                }

                @Override
                public void onErrorEvent(int i, int i1) {

                }
            }, VendorProperty.LANE_CENTERING_WARNING_EXTENDED_INDICATION_REQUEST, 0);

            mPropertyManager.registerCallback(new CarPropertyManager.CarPropertyEventCallback() {
                @Override
                public void onChangeEvent(CarPropertyValue carPropertyValue) {
                    if (carPropertyValue == null) {
                        Logger.i(TAG, "PREDICTED_FUEL_SAVING_PER_100_KM", "carPropertyValue == null");
                        return;
                    }
                    Integer value = (Integer) carPropertyValue.getValue();
                    Logger.i(TAG, "PREDICTED_FUEL_SAVING_PER_100_KM", value);
                    for (SignalAdapterCallback callback : mCallbacks) {
                        callback.onPredictedFuelSavingPer100km(value);
                    }
                }

                @Override
                public void onErrorEvent(int i, int i1) {
                }
            }, PatacProperty.PREDICTED_FUEL_SAVING_PER_100_KM, 0);

            mPropertyManager.registerCallback(new CarPropertyManager.CarPropertyEventCallback() {
                @Override
                public void onChangeEvent(CarPropertyValue carPropertyValue) {
                    if (carPropertyValue == null) {
                        Logger.i(TAG, "TOTAL_FUEL_SAVING", "carPropertyValue == null");
                        return;
                    }
                    Integer value = (Integer) carPropertyValue.getValue();
                    Logger.i(TAG, "TOTAL_FUEL_SAVING", value);
                    for (SignalAdapterCallback callback : mCallbacks) {
                        callback.onTotalFuelSaving(value);
                    }
                }

                @Override
                public void onErrorEvent(int i, int i1) {
                }
            }, PatacProperty.TOTAL_FUEL_SAVING, 0);
        } else {
            Logger.i(TAG, "mPropertyManager initialized");
        }
    }

    private void patacPropertyRegister(int patacProperty, int ttsValue) {
        mPropertyManager.registerCallback(new CarPropertyManager.CarPropertyEventCallback() {
            @Override
            public void onChangeEvent(CarPropertyValue carPropertyValue) {
                if (carPropertyValue == null) {
                    Logger.i(TAG, "carPropertyValue == null: " + ttsValue);
                    return;
                }
                Boolean value = (Boolean) carPropertyValue.getValue();
                Logger.d(TAG, ttsValue + " value: " + value);
                if (value) {
                    for (SignalAdapterCallback callback : mCallbacks) {
                        callback.onNaviOnADASStateChanged(ttsValue);
                    }
                }
            }

            @Override
            public void onErrorEvent(int i, int i1) {
                Logger.i(TAG, ttsValue, i, i1);
            }
        }, patacProperty, 0);
    }

    private void registerDYN(int var1, int var2, DynCallback callback) {
        Logger.i(TAG, "registerDYN: ", var1, var2);
        mPropertyManager.registerCallback(new CarPropertyManager.CarPropertyEventCallback() {
            @Override
            public void onChangeEvent(CarPropertyValue carPropertyValue) {
                if (carPropertyValue == null) {
                    Logger.i(TAG, "carPropertyValue == null");
                    return;
                }
                if (!(carPropertyValue.getValue() instanceof Integer[])) {
                    Logger.i(TAG, "value not Integer[]");
                    return;
                }
                Integer[] value = (Integer[]) carPropertyValue.getValue();
                if (value.length >= 3 && value[0] == var1 && value[1] == var2) {
                    callback.onDynChanged(value[2]);
                }
            }

            @Override
            public void onErrorEvent(int i, int i1) {
                Logger.i(TAG, "onErrorEvent: " + i + ", " + i1);
            }
        }, VendorProperty.ODI_DYNDATA, 0);
        mPropertyManager.setProperty(Integer[].class, VendorProperty.ODI_SUBSCRIBE, VehicleArea.GLOBAL, new Integer[]{var1, var2});
    }

    interface DynCallback {
        void onDynChanged(Integer value);
    }

    /**
     * 重置CarPropertyManager
     */
    private synchronized void reset() {
        this.mPropertyManager = null;
    }

    @Override
    public void registerCallback(final String key, final SignalAdapterCallback callback) {
        if (!mCallbacks.contains(callback)) {
            mCallbacks.add(callback);
        }
    }

    @Override
    public int getChargeSystemStatus() {
        Logger.d(TAG, "getChargeSystemStatus: " + mChargeSystemStatus);
        return mChargeSystemStatus;
    }

    @Override
    public float getBatteryEnergyPercent() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryStateOfChargeBatteryStateOfCharge();
        } catch (Exception e) {
            Logger.e(TAG, "getBatteryEnergyPercent: " + e.getMessage());
            return -1;
        }
        final Float value = result.getValue(-1f);
        Logger.d(TAG, "getBatteryEnergyPercent: " + value);
        return value;
    }

    @Override
    public float getMaxBatteryEnergy() {
        if (VehicleController.isCleaArch()) {
            float value = mBatteryEnergy / mBatteryEnergyPercent;
            Logger.d(TAG, "getMaxBatteryEnergy: ", value);
            return value;
        } else {
            Logger.d(TAG, "getMaxBatteryEnergy: ", maxBatteryEnergy);
            return maxBatteryEnergy;
        }
    }

    @Override
    public float getBatteryEnergy() {
        if (Logger.isDebugLevel()) Logger.d(TAG, "getBatteryEnergy: " + mBatteryEnergy);
        return mBatteryEnergy;
    }

    @Override
    public float getOutsideTemperature() {
        if (Logger.isDebugLevel()) Logger.d(TAG, "getOutsideTemperature: " + mOutsideTemperature);
        return mOutsideTemperature;
    }

    @Override
    public float getSpeedOfVehicle() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getSpeedOfVehicle();
        } catch (Exception e) {
            Logger.e(TAG, "getSpeedOfVehicle: " + e.getMessage());
            return -1;
        }
        final Float value = result.getValue(-1f);
        Logger.d(TAG, "getSpeedOfVehicle: " + value);
        return value;
    }

    @Override
    public int getAcSwitchState() {
        final VehicleController.Result<Integer> result;
        try {
            result = HvacController.getInstance().getAcSwitchState(HvacController.FIRST_ROW_LEFT_SEAT);
        } catch (Exception e) {
            Logger.e(TAG, "getAcSwitchState: " + e.getMessage());
            return -1;
        }
        final Integer value = result.getValue(-1);
        Logger.d(TAG, "getAcSwitchState: " + value);
        return value;
    }

    @Override
    public int getSystemState() {
        final int result;
        try {
            result = PowerModeManager.getInstance().getSystemState();
        } catch (Exception e) {
            Logger.e(TAG, "getSystemState: " + e.getMessage());
            return -1;
        }
        Logger.d(TAG, "getSystemState: " + result);
        return systemStateConversion(result);
    }

    @Override
    public float getRangeRemaining() {
        Logger.d(TAG, "getRangeRemaining: " + mRangeRemaining);
        return mRangeRemaining;
    }

    @Override
    public float getHighVoltageBatteryPropulsionRange() {
        Logger.d(TAG, "getHighVoltageBatteryPropulsionRange: " + mHighVoltageBatteryPropulsionRange);
        return mHighVoltageBatteryPropulsionRange;
    }

    @Override
    public void setNextChargingDestination(final int powerLevel, final int status, final int timeToArrival, final int distToArrival) {
        if (mPropertyManager == null) {
            Logger.w(TAG, "setNextChargingDestination: mPropertyManager is null");
            return;
        }
        Logger.d(TAG, "setNextChargingDestination: " + powerLevel + ", " + status + ", " + timeToArrival + ", " + distToArrival);
        final Integer[] nextChargingDestination = new Integer[]{distToArrival, status, timeToArrival, powerLevel};
        try {
            mPropertyManager.setProperty(Integer[].class, VendorProperty.NEXT_CHARGING_DESTINATION_INFORMATION_1, VehicleArea.GLOBAL, nextChargingDestination);
        } catch (Exception e) {
            Logger.e(TAG, "setNextChargingDestination: " + e.getMessage());
        }
    }

    @Override
    public boolean getGmcNopTtsEnabled() {
        if (!VehicleController.isCleaArch()) {
            return false;
        }
        final Integer result;
        try {
            CarPropertyValue<Integer> property = mPropertyManager.getProperty(Integer.class, PatacProperty.NAVIGATION_ON_ADAS_INDICATION_REQUEST
                    , VehicleArea.GLOBAL);
            result = property.getValue();
        } catch (Exception e) {
            Logger.i(TAG, "getGmcNopTtsEnabled: " + e.getMessage());
            return false;
        }
        Logger.d(TAG, "getGmcNopTtsEnabled: " + result);
        return result == 2;
    }

    @Override
    public boolean getPatacNopTtsEnabled() {
        if (!VehicleController.isCleaArch()) {
            return false;
        }
        final Integer result;
        try {
            CarPropertyValue<Integer> property = mPropertyManager.getProperty(Integer.class, VendorProperty.LANE_CENTERING_CONTROL_INDICATION_REQUEST
                    , VehicleArea.GLOBAL);
            result = property.getValue();
        } catch (Exception e) {
            Logger.i(TAG, "getPatacNopTtsEnabled: " + e.getMessage());
            return false;
        }
        Logger.d(TAG, "getPatacNopTtsEnabled: " + result);
        return result == 2;
    }

    //region 音量相关接口
    public void initVolumeCallback(Car car) {
        mCarAudioManager = (CarAudioManager) car.getCarManager(Car.AUDIO_SERVICE);
        mCarAudioManager.registerCarVolumeCallback(new CarAudioManager.CarVolumeCallback() {
            @Override
            public void onGroupVolumeChanged(int zoneId, int groupId, int flags) {
                Logger.d(TAG,  zoneId, groupId, flags);
                if (zoneId == CarAudioManager.PRIMARY_AUDIO_ZONE && groupId == getNaviGroupId()) {
                    int volume = mCarAudioManager.getGroupVolume(CarAudioManager.PRIMARY_AUDIO_ZONE, getNaviGroupId());
                    Logger.d(TAG, volume);
                    for (SignalAdapterCallback callback : mCallbacks) {
                        callback.onNaviVolumeChanged(volume);
                    }
                }
            }

            @Override
            public void onMasterMuteChanged(int zoneId, int flags) {
            }
            @Override
            public void onGroupMuteChanged(int zoneId, int groupId, int flags) {
            }
        });
    }

    @Override
    public void setNaviVolume(int volume) {
        Logger.d(TAG, volume);
        if (mCarAudioManager == null) {
            return;
        }
        mCarAudioManager.setGroupVolume(CarAudioManager.PRIMARY_AUDIO_ZONE, getNaviGroupId(), volume, 0);
    }

    @Override
    public int getNaviVolume() {
        if (mCarAudioManager == null) {
            return -1;
        }
        int groupVolume = mCarAudioManager.getGroupVolume(CarAudioManager.PRIMARY_AUDIO_ZONE, getNaviGroupId());
        Logger.d(TAG, groupVolume);
        return groupVolume;
    }

    private int getNaviGroupId() {
        if (VehicleController.isCleaArch()) {
            return 2;
        } else {
            return 1;
        }
    }
    //endregion

    //region clea can功能接口
    @Override
    public void setSdNavigationStatus(SdNavigationStatusGroup sdNavigationStatusGroup) {
        if (Logger.isDebugLevel()) Logger.d(TAG, sdNavigationStatusGroup.toString());
        try {
            mPropertyManager.setProperty(Integer[].class, PatacProperty.SD_NAVIGATION_STATUS_GROUP,
                    VehicleArea.GLOBAL, sdNavigationStatusGroup.toArray());
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setNavigationOnAdasButtonSettingRequest(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.NAVIGATION_ON_ADAS_BUTTON_SETTING_REQUEST,
                    VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setNavigationOnAdasInfoNavigationStatus(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.NAVIGATION_ON_ADAS_INFO_NAVIGATION_STATUS,
                    VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setDistanceToTrafficJamRoad(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.DISTANCE_TO_TRAFFIC_JAM_ROAD
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setDistanceToTrafficJamRoadAvailability(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.DISTANCE_TO_TRAFFIC_JAM_ROAD_AVAILABILITY
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setDistanceOnTrafficJamRoad(float value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Float.class, PatacProperty.DISTANCE_ON_TRAFFIC_JAM_ROAD
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setDistanceOnTrafficJamRoadAvailability(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.DISTANCE_ON_TRAFFIC_JAM_ROAD_AVAILABILITY
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setTrafficJamRoadAverageSpeed(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.TRAFFIC_JAM_ROAD_AVERAGE_SPEED
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setTrafficJamRoadAverageSpeedAvailability(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.TRAFFIC_JAM_ROAD_AVERAGE_SPEED_AVAILABILITY
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setRoadConditionGroup(RoadConditionGroup roadConditionGroup) {
        if (Logger.isDebugLevel()) Logger.d(TAG, roadConditionGroup);
        int propertyId = 0;
        if (Objects.equals(BuildConfig.FLAVOR, "clea_local_8155")) {
            propertyId = 557928600;
        } else {
            propertyId = 557928590;
        }
        // PatacProperty.ROAD_CONDITION_GROUP
        try {
            mPropertyManager.setProperty(Integer[].class, propertyId
                    , VehicleArea.GLOBAL, roadConditionGroup.toArray());
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setTotalDistanceFromStartToDestinationOnNavigation(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.TOTAL_DISTANCE_FROM_START_TO_DESTINATION_ON_NAVIGATION
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setTotalPredictedTimeFromStartToDestinationOnNavigation(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.TOTAL_PREDICTED_TIME_FROM_START_TO_DESTINATION_ON_NAVIGATION
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setRemainDistanceToChargingStation(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.REMAIN_DISTANCE_TO_CHARGING_STATION
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setRemainTimeToChargingStationy(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.REMAIN_TIME_TO_CHARGING_STATION
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setVcuSpeedLimitArbitrationResults(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.VCU_SPEED_LIMIT_ARBITRATION_RESULTS
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void setVcuSpeedLimitArbitrationResultsAssured(int value) {
        Logger.d(TAG, value);
        try {
            mPropertyManager.setProperty(Integer.class, PatacProperty.VCU_SPEED_LIMIT_ARBITRATION_RESULTS_ASSURED
                    , VehicleArea.GLOBAL, value);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public int getPredictedFuelSavingPer100km() {
        Logger.d(TAG, mPredictedFuelSavingPer100km);
        return mPredictedFuelSavingPer100km;
    }

    @Override
    public int getTotalFuelSaving() {
        Logger.d(TAG, mTotalFuelSaving);
        return mTotalFuelSaving;
    }
    //endregion

    /**
     * 状态转换
     *
     * @param state
     * @return int
     */
    private int systemStateConversion(final int state) {
        switch (state) {
            case PowerModeManager.STATE_LOCALINFOTAINMENT: // OFF
                return 0;
            case PowerModeManager.STATE_ACCESSORY: // ACC
                return 1;
            case PowerModeManager.STATE_RUN: // RUN
                return 2;
            case PowerModeManager.STATE_START: // START
                return 3;
            case PowerModeManager.STATE_SLEEP: // SLEEP
                return 4;
            case PowerModeManager.STATE_PROPULSION: // SLEEP
                return 5;
            case PowerModeManager.STATE_HMIINACTIVE: // HMIINACTIVE
                return 6;
            default:
                return -1;
        }
    }
}
