package com.fy.navi.hmi.setting.guide.platenumber;

import com.fy.navi.service.logicpaket.setting.SettingCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.base.BaseModel;

public class SettingPlateNumberModel extends BaseModel<SettingPlateNumberViewModel> implements SettingCallback {

    private final SettingPackage mSettingPackage;


    public SettingPlateNumberModel() {
        mSettingPackage = SettingPackage.getInstance();
    }
    @Override
    public void onCreate() {
        super.onCreate();
        mSettingPackage.registerCallBack("SettingPlateNumberModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void notify(final int eventType, final int exCode) {

    }

    public String getPlateNumber() {
        return mSettingPackage.getConfigKeyPlateNumber();
    }

    /**
     * 设置车牌号
     * @param plateNumber 车牌号
     */
    public void setPlateNumber(final String plateNumber) {
        mSettingPackage.setConfigKeyPlateNumber(plateNumber);
    }

    /**
     * 设置是否限行
     * @param avoidLimit 是否限行
     */
    public void setAvoidLimit(final boolean avoidLimit) {
        mSettingPackage.setConfigKeyAvoidLimit(avoidLimit);
    }
}
