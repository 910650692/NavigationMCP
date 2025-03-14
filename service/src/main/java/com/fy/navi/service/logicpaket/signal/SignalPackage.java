package com.fy.navi.service.logicpaket.signal;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.adapter.signal.SignalAdapter;
import com.fy.navi.service.adapter.signal.SignalAdapterCallback;

import java.util.Hashtable;

public class SignalPackage implements SignalAdapterCallback {
    public static final String TAG = SignalPackage.class.getSimpleName();

    private final SignalAdapter mSignalAdapter;
    private Hashtable<String, SignalCallback> mSignalCallbacks;

    public static SignalPackage getInstance() {
        return SInstanceHolder.sInstance;
    }

    private static final class SInstanceHolder {
        static final SignalPackage sInstance = new SignalPackage();
    }

    private SignalPackage() {
        mSignalCallbacks = new Hashtable<>();
        mSignalAdapter = SignalAdapter.getInstance();
    }

    public void init(Context context) {
        mSignalAdapter.initSignal(context);
        mSignalAdapter.registerCallback(TAG, this);
    }

    /**
     * 注册回调
     */
    public void registerObserver(String key, SignalCallback signalCallback) {
        mSignalCallbacks.put(key, signalCallback);
    }

    /**
     * 取消回调
     */
    public void unregisterObserver(String key) {
        mSignalCallbacks.remove(key);
    }

    @Override
    public void onSpeedChanged(float speed) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onSpeedChanged(speed);
                    }
                }
            }
        });
    }

    @Override
    public void onGearChanged(int gear) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onGearChanged(gear);
                    }
                }
            }
        });
    }

    @Override
    public void onSystemStateChanged(int state) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onSystemStateChanged(state);
                    }
                }
            }
        });
    }

    /**
     * 车外温度
     *
     * @return 单位°C
     */
    public float getOutsideTemperature() {
        return mSignalAdapter.getOutsideTemperature();
    }

    /**
     * 车速
     *
     * @return 单位m/s
     */
    public float getSpeedOfVehicle() {
        return mSignalAdapter.getSpeedOfVehicle();
    }

    /**
     * 电池剩余电量
     *
     * @return 单位kWh
     */
    public float getBatteryEnergy() {
        return mSignalAdapter.getBatteryEnergy();
    }

    /**
     * 电池剩余电量百分比
     *
     * @return 单位%
     */
    public float getBatteryEnergyPercent() {
        return mSignalAdapter.getBatteryEnergyPercent();
    }

    /**
     * 电池最大电量
     *
     * @return 单位kWh
     */
    public float getMaxBatteryEnergy() {
        return mSignalAdapter.getMaxBatteryEnergy();
    }

    /**
     * 充电系统状态
     *
     * @return 充电状态
     * DEFAULT = 0;
     * IDLE = 1;
     * INITIALIZING = 2;
     * ACTIVE = 3;
     * COMPLETE = 4;
     * ABORTED = 5;
     * UTILITY_OVERRIDE_ACTIVE = 6;
     * UTILITY_OVERRIDE_REDUCED_POWER = 7;
     * PAUSE_DUE_TO_UPDATE = 8;
     * CONNECTION_UNPOWERED = 9;
     * UNCONNECTED = 10;
     * OFFBOARD_ENERGY_TRANSFER_ACTIVE = 11;
     */
    public int getChargeSystemStatus() {
        return mSignalAdapter.getChargeSystemStatus();
    }

    /**
     * 空调开关状态
     *
     * @return 0:关闭 1:开启
     */
    public int getAcSwitchState() {
        return mSignalAdapter.getAcSwitchState();
    }

    /**
     * 系统状态
     * @return 系统状态
     * OFF = 0
     * ACC = 1
     * RUN = 2
     * CRANK(START) = 3
     * SLEEP = 4
     */
    public int getSystemState() {
        return mSignalAdapter.getSystemState();
    }

}