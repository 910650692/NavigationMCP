package com.fy.navi.hmi.search.mainsearch;

import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.BaseModel;

public class MainSearchModel extends BaseModel<MainSearchViewModel> {
    private final CalibrationPackage mCalibrationPackage;

    public MainSearchModel() {
        mCalibrationPackage = CalibrationPackage.getInstance();
    }


    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     */
    public int powerType() {
        return mCalibrationPackage.powerType();
    }
}
