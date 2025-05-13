package com.fy.navi.service.adapter.signal.bls;

import android.car.Car;
import android.car.hardware.property.CarPropertyManager;
import android.content.Context;
import android.os.Build;
import android.os.Handler;
import android.os.HandlerThread;
import android.util.Log;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.signal.SignalAdapterCallback;
import com.fy.navi.service.adapter.signal.SignalApi;
import com.fy.navi.service.define.signal.SignalConst;
import com.patac.vehicle.DriveAssistController;
import com.patac.vehicle.HvacController;
import com.patac.vehicle.PowertainController;
import com.patac.vehicle.VehicleController;
import com.patac.vehicle.VehicleStatusController;

import java.util.List;
import java.util.NoSuchElementException;
import java.util.concurrent.CopyOnWriteArrayList;

import gm.powermode.PowerModeManager;
import vendor.gm.vehicle.V1_0.VendorProperty;

public class SignalAdapterImpl implements SignalApi {
    private static final String TAG = MapDefaultFinalTag.SIGNAL_SERVICE_TAG;

    private HandlerThread mWorkThread;
    private VehicleController mVehicleController;
    private final List<SignalAdapterCallback> mCallbacks = new CopyOnWriteArrayList<>();
    private Car mCar;
    private CarPropertyManager mPropertyManager;

    public SignalAdapterImpl() {
    }

    /**
     * 初始化回调函数
     */
    private void initCallback() {
        PowertainController.getInstance().registerSpeedOfVehicleListener(new PowertainController.SpeedOfVehicleListener() {
            @Override
            public void onSpeedOfVehicleSignalChanged(final Float speed) {
                Logger.d(TAG, "onSpeedOfVehicleSignalChanged: " + speed);
                for (SignalAdapterCallback callback : mCallbacks) {
                    callback.onSpeedChanged(speed);
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
        PowertainController.getInstance().registerRangeRemainingListener(new PowertainController.RangeRemainingListener() {
            @Override
            public void onRangeRemainingSignalChanged(final Float value) {
                Logger.d(TAG, "onRangeRemainingSignalChanged: " + value);
                for (SignalAdapterCallback callback : mCallbacks) {
                    callback.onRangeRemainingSignalChanged(value);
                }
            }
        });
        PowertainController.getInstance().registerHighVoltageBatteryPropulsionRangeListener(
                new PowertainController.HighVoltageBatteryPropulsionRangeListener() {
                    @Override
                    public void onHighVoltageBatteryPropulsionRangeChanged(final float value) {
                        Logger.d(TAG, "onHighVoltageBatteryPropulsionRangeChanged: " + value);
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
        DriveAssistController.getInstance().registerLaneCenteringWarningIndicationRequestIdcmAListener(
                new DriveAssistController.LaneCenteringWarningIndicationRequestIdcmAListener() {
                    @Override
                    public void onLaneCenteringWarningIndicationRequestIdcmAChanged(final int state) {
                        Logger.d(TAG, state);
                        for (SignalAdapterCallback callback : mCallbacks) {
                            callback.onLaneCenteringWarningIndicationRequestIdcmAChanged(state);
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
        if (!checkVehicleEnvironment()) {
            return;
        }
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
        Logger.d(TAG, "init: success");

        mCar = Car.createCar(context, handler, 5000L, this.mCarServiceLifecycleListener);
        if (mCar != null && mCar.isConnected()) {
            Logger.d(TAG, "Car connect successfully in VehicleService constructor");
            initPropertyManager(mCar);
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
        } else {
            Logger.i(TAG, "mPropertyManager in VehicleService had been inited");
        }
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
        final VehicleController.Result<Integer> result;
        try {
            result = PowertainController.getInstance().getHighVoltageChargeSystemStatus();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getChargeSystemStatus: " + Log.getStackTraceString(e));
            return -1;
        }
        final Integer value = result.getValue(-1);
        Logger.d(TAG, "getChargeSystemStatus: " + value);
        return value;
    }

    @Override
    public float getBatteryEnergyPercent() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryStateOfChargeBatteryStateOfCharge();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getBatteryEnergyPercent: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Logger.d(TAG, "getBatteryEnergyPercent: " + value);
        return value;
    }

    @Override
    public float getMaxBatteryEnergy() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getInfoEvBatteryCapacity();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.w(TAG, "getMaxBatteryEnergy: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Logger.d(TAG, "getMaxBatteryEnergy: " + value);
        return value;
    }

    @Override
    public float getBatteryEnergy() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryRemainingUsableEnergy();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getBatteryEnergy: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Logger.d(TAG, "getBatteryEnergy: " + value);
        return value;
    }

    @Override
    public float getOutsideTemperature() {
        final VehicleController.Result<Float> result;
        try {
            result = VehicleStatusController.getInstance().getOutsideTemperature();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getOutsideTemperature: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Logger.d(TAG, "getOutsideTemperature: " + value);
        return value;
    }

    @Override
    public float getSpeedOfVehicle() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getSpeedOfVehicle();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getSpeedOfVehicle: " + Log.getStackTraceString(e));
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
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getAcSwitchState: " + Log.getStackTraceString(e));
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
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getSystemState: " + Log.getStackTraceString(e));
            return -1;
        }
        Logger.d(TAG, "getSystemState: " + result);
        return systemStateConversion(result);
    }

    @Override
    public float getRangeRemaining() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getRangeRemaining();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getRangeRemaining: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Logger.d(TAG, "getRangeRemaining: " + value);
        return value;
    }

    @Override
    public float getHighVoltageBatteryPropulsionRange() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryPropulsionRange();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getHighVoltageBatteryPropulsionRange: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Logger.d(TAG, "getHighVoltageBatteryPropulsionRange: " + value);
        return value;
    }

    @Override
    public void setNextChargingDestination(final int powerLevel, final int status, final int timeToArrival, final int distToArrival) {
        if (mPropertyManager == null) {
            Logger.w(TAG, "setNextChargingDestination: mPropertyManager is null");
            return;
        }
        Logger.d(TAG, "setNextChargingDestination: " + powerLevel + ", " + status + ", " + timeToArrival + ", " + distToArrival);
        final int[] nextChargingDestination = new int[]{distToArrival, status, timeToArrival, powerLevel};
        try {
            mPropertyManager.setProperty(int[].class, VendorProperty.NEXT_CHARGING_DESTINATION_INFORMATION_1, 0, nextChargingDestination);
        } catch (IllegalArgumentException e) {
            Logger.e(TAG, "setNextChargingDestination: " + Log.getStackTraceString(e));
        }
    }

    @Override
    public int getNavigationOnAdasTextToSpeachStatus() {
        final VehicleController.Result<Integer> result;
        try {
            result = DriveAssistController.getInstance().getNavigationOnAdasTextToSpeachStatus();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Logger.e(TAG, "getNavigationOnAdasTextToSpeachStatus: " + Log.getStackTraceString(e));
            return -1;
        }
        final Integer value = result.getValue(-1);
        Logger.d(TAG, "getNavigationOnAdasTextToSpeachStatus: " + value);
        return value;
    }

    /**
     * 检查设备是否为GM车机
     *
     * @return boolean
     */
    private boolean checkVehicleEnvironment() {
        if (!"gm".equals(Build.MANUFACTURER)) { // 设备制造商
            Logger.w(TAG, "checkVehicleEnvironment: not vehicle environment");
            return false;
        } else {
            return true;
        }
    }

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
            default:
                return -1;
        }
    }
}
