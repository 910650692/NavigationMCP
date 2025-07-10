package com.sgm.navi.hmi.setting.others.privacy;

import static com.sgm.navi.service.MapDefaultFinalTag.NAVI_EXIT;

import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.ui.base.BaseModel;

public class SettingOthersPrivacyModel extends BaseModel<SettingOthersPrivacyViewModel>
        implements SettingUpdateObservable.SettingUpdateObserver, NaviStatusCallback {

    private static final String TAG = SettingOthersPrivacyModel.class.getSimpleName();
    private final SettingPackage mSettingPackage;

    public SettingOthersPrivacyModel() {
        mSettingPackage = SettingPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        SettingUpdateObservable.getInstance().addObserver(TAG, SettingOthersPrivacyModel.this);
        NaviStatusPackage.getInstance().registerObserver(TAG, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        SettingUpdateObservable.getInstance().removeObserver(TAG, SettingOthersPrivacyModel.this);
        NaviStatusPackage.getInstance().unregisterObserver(TAG);
    }

    /**
     * 初始化各设置项状态值
     */
    public void initView() {
        final boolean isOneYearPrivacy = mSettingPackage.getPrivacyStatus();
        final String endDate = mSettingPackage.getEndDate();
        mViewModel.dualChoiceControl(isOneYearPrivacy, endDate);
    }

    /**
     * 设置隐私状态.
     *
     * @param isOneYearPrivacy true:已授权一年，false:未授权
     *
     */
    public void setPrivacyStatus(final boolean isOneYearPrivacy) {
        mSettingPackage.setPrivacyStatus(isOneYearPrivacy);
    }

    @Override
    public void onUpdateSetting(String key, boolean value) {
        if (SettingController.KEY_SETTING_PRIVACY_STATUS.equals(key)) {
            ThreadManager.getInstance().postUi(this::initView);
        }
    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        if (!mSettingPackage.getPrivacyStatus() && TextUtils.equals(naviStatus, NaviStatus.NaviStatusType.NO_STATUS)) {
            if (Logger.openLog) {
                Logger.printStackTrace(NAVI_EXIT,true);
            }
            System.exit(0);
        }
    }

}
