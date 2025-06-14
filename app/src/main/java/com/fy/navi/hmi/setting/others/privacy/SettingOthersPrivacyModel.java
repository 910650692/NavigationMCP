package com.fy.navi.hmi.setting.others.privacy;


import android.text.TextUtils;

import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.setting.SettingCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.base.BaseModel;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class SettingOthersPrivacyModel extends BaseModel<SettingOthersPrivacyViewModel> implements SettingCallback, NaviStatusCallback {

    private final SettingPackage mSettingPackage;

    public SettingOthersPrivacyModel() {
        mSettingPackage = SettingPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mSettingPackage.registerCallBack("SettingOthersPrivacyModel",this);
        NaviStatusPackage.getInstance().registerObserver("SettingOthersPrivacyModel", this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSettingPackage != null) {
            mSettingPackage.unRegisterCallBack("SettingOthersPrivacyModel");
        }
    }

    /**
     * 初始化各设置项状态值
     */
    public void initView() {
        setPrivacyStatus();
        setEndDate();
    }

    /**
     * 设置隐私状态
     */
    private void setPrivacyStatus() {
        final boolean isOneYearPrivacy = mSettingPackage.getPrivacyStatus();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_PRIVACY_STATUS, isOneYearPrivacy);
    }

    /**
     * 设置隐私状态
     * @param isOneYearPrivacy true:已授权一年，false:未授权
     */
    public void setPrivacyStatus(final boolean isOneYearPrivacy) {
        mSettingPackage.setPrivacyStatus(isOneYearPrivacy);
    }


    /**
     * 设置隐私到期时间
     */
    public void setEndDate() {
        mViewModel.setEndDate(mSettingPackage.getEndDate());
    }

    /**
     * 设置隐私到期时间
     * @param endDateTime 到期时间
     */
    public void setEndDate(final String endDateTime) {
       mSettingPackage.setEndDate(endDateTime);
    }

    /**
     * 获取隐私到期时间
     * @return 到期时间
     */
    public String getEndDate() {
        return mSettingPackage.getEndDate();
    }


    /**
     * 格式化日期
     * @return 格式化后的日期
     */
    public String getFormattedDate() {
        // 获取当前日期
        final LocalDate currentDate = LocalDate.now();
        // 获取一年后的日期
        final LocalDate oneYearLater = currentDate.plusYears(1);
        // 定义日期格式
        final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy年M月d日");

        return oneYearLater.format(formatter);
    }

    @Override
    public void notify(final int eventType, final int exCode) {

    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        if (!mSettingPackage.getPrivacyStatus() && TextUtils.equals(naviStatus, NaviStatus.NaviStatusType.NO_STATUS)) {
            System.exit(0);
        }
    }
}
