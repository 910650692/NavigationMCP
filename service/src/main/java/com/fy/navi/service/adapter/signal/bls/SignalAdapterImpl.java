package com.fy.navi.service.adapter.signal.bls;

import android.content.Context;
import android.os.Build;
import android.os.Handler;
import android.os.HandlerThread;
import android.util.Log;

import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.signal.SignalAdapterCallback;
import com.fy.navi.service.adapter.signal.SignalApi;
import com.patac.vehicle.HvacController;
import com.patac.vehicle.PowertainController;
import com.patac.vehicle.VehicleController;
import com.patac.vehicle.VehicleStatusController;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import gm.powermode.PowerModeManager;

public class SignalAdapterImpl implements SignalApi {
    private static final String TAG = MapDefaultFinalTag.SIGNAL_SERVICE_TAG;

    private HandlerThread mWorkThread;
    private VehicleController mVehicleController;
    private final List<SignalAdapterCallback> mCallbacks = new CopyOnWriteArrayList<>();

    public SignalAdapterImpl() {
    }

    private abstract class VehicleSpeedListener extends VehicleController.SignalListener<Float> {
        public VehicleSpeedListener() {
            super(291504647, 0, 1.0f);
        }

        protected void onSignalChanged(int area, Float value) {
            this.onVehicleSpeedChanged(value);
        }

        public abstract void onVehicleSpeedChanged(Float speed);
    }

    private void initCallback() {
        PowertainController.getInstance().registerSpeedOfVehicleListener(new PowertainController.SpeedOfVehicleListener() {
            @Override
            public void onSpeedOfVehicleSignalChanged(Float speed) {
                Log.d(TAG, "onSpeedOfVehicleSignalChanged: " + speed);
                for (SignalAdapterCallback callback : mCallbacks) {
                    callback.onSpeedChanged(speed);
                }
            }
        });
        PowertainController.getInstance().registerVehicleMotionMovementStateListener(new PowertainController.VehicleMotionMovementStateListener() {
            @Override
            public void onVehicleMotionMovementStateChanged(Integer gear) {
                Log.d(TAG, "onVehicleMotionMovementStateChanged: " + gear);
                for (SignalAdapterCallback callback : mCallbacks) {
                    callback.onGearChanged(gear);
                }
            }
        });
        PowerModeManager.getInstance().registerSystemStateListener((newMode, oldMode) -> {
            Log.d(TAG, "onSystemStateChanged: oldMode = " + oldMode + ", newMode = " + newMode);
            for (SignalAdapterCallback callback : mCallbacks) {
                callback.onSystemStateChanged(systemStateConversion(newMode));
            }
            return 0;
        });
    }

    @Override
    public void initSignal(Context context) {
        if (!checkVehicleEnvironment()) {
            return;
        }
        mWorkThread = new HandlerThread("can-thread");
        mWorkThread.start();

        Handler handler = new Handler(mWorkThread.getLooper());

        /**
         * 首先调用VehicleController.getInstance获得VehicleController实例.
         * 此class对象负责和CarService建议连接，重连等工作.
         * 在初始化VehicleController的时候，如果传递了具体的Handler，后续的Event处理将会在此Handler所绑定的Looper中处理；
         * 如果传递的Handler是null，后续的Event处理将会主线程Looper中处理 */
        mVehicleController = VehicleController.getInstance(context, handler);
        initCallback();
        Log.d(TAG, "init: success");
    }

    @Override
    public void registerCallback(String key, SignalAdapterCallback callback) {
        if (!mCallbacks.contains(callback)) {
            mCallbacks.add(callback);
        }
    }

    @Override
    public int getChargeSystemStatus() {
        VehicleController.Result<Integer> result;
        try {
            result = PowertainController.getInstance().getHighVoltageChargeSystemStatus();
        } catch (Exception e) {
            Log.e(TAG, "getChargeSystemStatus: " + Log.getStackTraceString(e));
            return -1;
        }
        Integer value = result.getValue(-1);
        Log.d(TAG, "getChargeSystemStatus: " + value);
        return value;
    }

    @Override
    public float getBatteryEnergyPercent() {
        VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryStateOfChargeBatteryStateOfCharge();
        } catch (Exception e) {
            Log.e(TAG, "getBatteryEnergyPercent: " + Log.getStackTraceString(e));
            return -1;
        }
        Float value = result.getValue(-1f);
        Log.d(TAG, "getBatteryEnergyPercent: " + value);
        return value;
    }

    @Override
    public float getMaxBatteryEnergy() {
        VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getInfoEvBatteryCapacity();
        } catch (Exception e) {
            Log.e(TAG, "getMaxBatteryEnergy: " + Log.getStackTraceString(e));
            return -1;
        }
        Float value = result.getValue(-1f);
        Log.d(TAG, "getMaxBatteryEnergy: " + value);
        return value;
    }

    @Override
    public float getBatteryEnergy() {
        VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryRemainingUsableEnergy();
        } catch (Exception e) {
            Log.e(TAG, "getBatteryEnergy: " + Log.getStackTraceString(e));
            return -1;
        }
        Float value = result.getValue(-1f);
        Log.d(TAG, "getBatteryEnergy: " + value);
        return value;
    }

    @Override
    public float getOutsideTemperature() {
        VehicleController.Result<Float> result;
        try {
            result = VehicleStatusController.getInstance().getOutsideTemperature();
        } catch (Exception e) {
            Log.e(TAG, "getOutsideTemperature: " + Log.getStackTraceString(e));
            return -1;
        }
        Float value = result.getValue(-1f);
        Log.d(TAG, "getOutsideTemperature: " + value);
        return value;
    }

    @Override
    public float getSpeedOfVehicle() {
        VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getSpeedOfVehicle();
        } catch (Exception e) {
            Log.e(TAG, "getSpeedOfVehicle: " + Log.getStackTraceString(e));
            return -1;
        }
        Float value = result.getValue(-1f);
        Log.d(TAG, "getSpeedOfVehicle: " + value);
        return value;
    }

    @Override
    public int getAcSwitchState() {
        VehicleController.Result<Integer> result;
        try {
            result = HvacController.getInstance().getAcSwitchState(HvacController.FIRST_ROW_LEFT_SEAT);
        } catch (Exception e) {
            Log.e(TAG, "getAcSwitchState: " + Log.getStackTraceString(e));
            return -1;
        }
        Integer value = result.getValue(-1);
        Log.d(TAG, "getAcSwitchState: " + value);
        return value;
    }

    @Override
    public int getSystemState() {
        int result;
        try {
            result = PowerModeManager.getInstance().getSystemState();
        } catch (Exception e) {
            Log.e(TAG, "getSystemState: " + Log.getStackTraceString(e));
            return -1;
        }
        Log.d(TAG, "getSystemState: " + result);
        return systemStateConversion(result);
    }

    private boolean checkVehicleEnvironment() {
        if (!"gm".equals(Build.MANUFACTURER)) { // 设备制造商
            Log.w(TAG, "checkVehicleEnvironment: not vehicle environment");
            return false;
        } else {
            return true;
        }
    }

    private int systemStateConversion(int state) {
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
            default:
                return -1;
        }
    }
}
