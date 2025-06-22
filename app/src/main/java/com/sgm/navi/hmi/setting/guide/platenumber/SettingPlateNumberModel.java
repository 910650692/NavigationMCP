package com.sgm.navi.hmi.setting.guide.platenumber;

import com.sgm.navi.service.logicpaket.setting.SettingCallback;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.ui.base.BaseModel;

public class SettingPlateNumberModel extends BaseModel<SettingPlateNumberViewModel> implements SettingCallback, SettingUpdateObservable.SettingUpdateObserver {

    private final SettingPackage mSettingPackage;
    private static final String MODEL_NAME = "SettingPlateNumberModel";


    public SettingPlateNumberModel() {
        mSettingPackage = SettingPackage.getInstance();
    }
    @Override
    public void onCreate() {
        super.onCreate();
        mSettingPackage.registerCallBack("SettingPlateNumberModel",this);
        SettingUpdateObservable.getInstance().addObserver(MODEL_NAME, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSettingPackage != null) {
            mSettingPackage.unRegisterCallBack("SettingPlateNumberModel");
        }
        SettingUpdateObservable.getInstance().removeObserver(MODEL_NAME, this);
    }

    @Override
    public void notify(final int eventType, final int exCode) {

    }

    @Override
    public void onPlateNumberChanged(String plateNumber) {
        mViewModel.onPlateNumberChanged(plateNumber);
    }

    public String getPlateNumber() {
        return mSettingPackage.getConfigKeyPlateNumber();
    }

    /**
     * 设置限行状态
     *
     * @param avoidLimit true 限行 false 不限行
     * @return code 0 成功 其他 失败
     */
    public int setConfigKeyAvoidLimit(final boolean avoidLimit) {
        return mSettingPackage.setConfigKeyAvoidLimit(avoidLimit);
    }

    /**
     * 设置车牌号
     *
     * @param carNumber 车牌号
     */
    public void setConfigKeyPlateNumber(final String carNumber) {
        mSettingPackage.setConfigKeyPlateNumber(carNumber);
    }

}
