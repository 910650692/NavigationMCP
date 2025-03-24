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
import java.util.NoSuchElementException;
import java.util.concurrent.CopyOnWriteArrayList;

import gm.powermode.PowerModeManager;

public class SignalAdapterImpl implements SignalApi {
    private static final String TAG = MapDefaultFinalTag.SIGNAL_SERVICE_TAG;

    private HandlerThread mWorkThread;
    private VehicleController mVehicleController;
    private final List<SignalAdapterCallback> mCallbacks = new CopyOnWriteArrayList<>();

    public SignalAdapterImpl() {
    }

    /**
     * 初始化回调函数
     */
    private void initCallback() {
        PowertainController.getInstance().registerSpeedOfVehicleListener(new PowertainController.SpeedOfVehicleListener() {
            @Override
            public void onSpeedOfVehicleSignalChanged(final Float speed) {
                Log.d(TAG, "onSpeedOfVehicleSignalChanged: " + speed);
                for (SignalAdapterCallback callback : mCallbacks) {
                    callback.onSpeedChanged(speed);
                }
            }
        });
        PowertainController.getInstance().registerVehicleMotionMovementStateListener(new PowertainController.VehicleMotionMovementStateListener() {
            @Override
            public void onVehicleMotionMovementStateChanged(final Integer gear) {
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
        Log.d(TAG, "init: success");
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
            Log.e(TAG, "getChargeSystemStatus: " + Log.getStackTraceString(e));
            return -1;
        }
        final Integer value = result.getValue(-1);
        Log.d(TAG, "getChargeSystemStatus: " + value);
        return value;
    }

    @Override
    public float getBatteryEnergyPercent() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryStateOfChargeBatteryStateOfCharge();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Log.e(TAG, "getBatteryEnergyPercent: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Log.d(TAG, "getBatteryEnergyPercent: " + value);
        return value;
    }

    @Override
    public float getMaxBatteryEnergy() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getInfoEvBatteryCapacity();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Log.e(TAG, "getMaxBatteryEnergy: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Log.d(TAG, "getMaxBatteryEnergy: " + value);
        return value;
    }

    @Override
    public float getBatteryEnergy() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getHighVoltageBatteryRemainingUsableEnergy();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Log.e(TAG, "getBatteryEnergy: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Log.d(TAG, "getBatteryEnergy: " + value);
        return value;
    }

    @Override
    public float getOutsideTemperature() {
        final VehicleController.Result<Float> result;
        try {
            result = VehicleStatusController.getInstance().getOutsideTemperature();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Log.e(TAG, "getOutsideTemperature: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Log.d(TAG, "getOutsideTemperature: " + value);
        return value;
    }

    @Override
    public float getSpeedOfVehicle() {
        final VehicleController.Result<Float> result;
        try {
            result = PowertainController.getInstance().getSpeedOfVehicle();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Log.e(TAG, "getSpeedOfVehicle: " + Log.getStackTraceString(e));
            return -1;
        }
        final Float value = result.getValue(-1f);
        Log.d(TAG, "getSpeedOfVehicle: " + value);
        return value;
    }

    @Override
    public int getAcSwitchState() {
        final VehicleController.Result<Integer> result;
        try {
            result = HvacController.getInstance().getAcSwitchState(HvacController.FIRST_ROW_LEFT_SEAT);
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Log.e(TAG, "getAcSwitchState: " + Log.getStackTraceString(e));
            return -1;
        }
        final Integer value = result.getValue(-1);
        Log.d(TAG, "getAcSwitchState: " + value);
        return value;
    }

    @Override
    public int getSystemState() {
        final int result;
        try {
            result = PowerModeManager.getInstance().getSystemState();
        } catch (IllegalStateException | NoSuchElementException | UnsupportedOperationException e) {
            Log.e(TAG, "getSystemState: " + Log.getStackTraceString(e));
            return -1;
        }
        Log.d(TAG, "getSystemState: " + result);
        return systemStateConversion(result);
    }

    /**
     * 检查设备是否为GM车机
     *
     * @return boolean
     */
    private boolean checkVehicleEnvironment() {
        if (!"gm".equals(Build.MANUFACTURER)) { // 设备制造商
            Log.w(TAG, "checkVehicleEnvironment: not vehicle environment");
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
            default:
                return -1;
        }
    }
}
