package com.fy.navi.hmi.setting.others.privacy;


import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.setting.SettingCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.base.BaseModel;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class SettingOthersPrivacyModel extends BaseModel<SettingOthersPrivacyViewModel> implements SettingCallback{

    private final SettingPackage settingPackage;

    public SettingOthersPrivacyModel() {
        settingPackage = SettingPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        settingPackage.registerCallBack("SettingOthersPrivacyModel",this);
    }

    /**
     * 初始化各设置项状态值
     */
    public void initView() {
        setPrivacyStatus();
        setEndDate();
    }

    private void setPrivacyStatus() {
        boolean isOneYearPrivacy = settingPackage.getPrivacyStatus();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_PRIVACY_STATUS, isOneYearPrivacy);
    }

    public void setPrivacyStatus(boolean isOneYearPrivacy) {
        settingPackage.setPrivacyStatus(isOneYearPrivacy);
    }


    public void setEndDate() {
        mViewModel.setEndDate(settingPackage.getEndDate());
    }

    public void setEndDate(String endDateTime) {
       settingPackage.setEndDate(endDateTime);
    }

    public String getEndDate() {
        return settingPackage.getEndDate();
    }


    public String getFormattedDate() {
        // 获取当前日期
        LocalDate currentDate = LocalDate.now();
        // 获取一年后的日期
        LocalDate oneYearLater = currentDate.plusYears(1);
        // 定义日期格式
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy年M月d日");

        return oneYearLater.format(formatter);
    }

    @Override
    public void notify(int eventType, int exCode) {

    }
}
