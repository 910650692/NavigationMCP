package com.fy.navi.service.logicpaket.calibration;

public @interface PowerType {
    /**
     * 默认值
     */
    int E_VEHICLE_ENERGY_DEFAULT = -1;
    /**
     * 燃油车
     */
    int E_VEHICLE_ENERGY_FUEL = 0;
    /**
     * 电动车
     */
    int E_VEHICLE_ENERGY_ELECTRIC = 1;
    /**
     * 混合动力
     */
    int E_VEHICLE_ENERGY_HYBRID = 2;
}
