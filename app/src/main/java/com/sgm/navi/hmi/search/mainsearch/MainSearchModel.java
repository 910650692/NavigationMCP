package com.sgm.navi.hmi.search.mainsearch;

import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.ui.base.BaseModel;

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
     * @return 动力类型
     */
    public int powerType() {
        return mCalibrationPackage.powerType();
    }
}
