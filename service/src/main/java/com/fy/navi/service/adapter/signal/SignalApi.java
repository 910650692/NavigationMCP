package com.fy.navi.service.adapter.signal;

import android.content.Context;

public interface SignalApi {

    /**
     * 初始化
     * @param context
     */
    void initSignal(Context context);

    /**
     * 注册回调
     *
     * @param key
     * @param resultCallback
     */
    void registerCallback(String key, SignalAdapterCallback resultCallback);

    /**
     * 车外温度
     *
     * @return 单位°C
     */
    float getOutsideTemperature();

    /**
     * 车速
     *
     * @return 单位m/s
     */
    float getSpeedOfVehicle();

    /**
     * 电池剩余电量
     *
     * @return 单位kWh
     */
    int getAcSwitchState();

    /**
     * 电池剩余电量百分比
     *
     * @return 单位%
     */
    int getChargeSystemStatus();

    /**
     * 电池最大电量
     *
     * @return 单位kWh
     */
    float getBatteryEnergyPercent();

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
    float getMaxBatteryEnergy();

    /**
     * 空调开关状态
     *
     * @return 0:关闭 1:开启
     */
    float getBatteryEnergy();

    /**
     * 系统状态
     * @return 系统状态
     * OFF = 0
     * ACC = 1
     * RUN = 2
     * CRANK(START) = 3
     * SLEEP = 4
     */
    int getSystemState();

    /**
     * 续航里程
     *
     * @return 单位km
     */
    float getRangeRemaining();

    /**
     * 高压电池续航里程
     *
     * @return 单位km
     */
    float getHighVoltageBatteryPropulsionRange();

    /**
     * 设置电池预加热参数
     * @param powerLevel int
     * @param status int
     * @param timeToArrival int
     */
    void setNextChargingDestination(int powerLevel, int status, int timeToArrival, int distToArrival);

    /**
     * 获取智慧领航播报开关
     * @return 0:关闭 1:开启
     */
    int getNavigationOnAdasTextToSpeachStatus();
}
