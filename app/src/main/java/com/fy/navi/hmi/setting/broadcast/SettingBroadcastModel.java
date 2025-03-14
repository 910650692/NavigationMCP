package com.fy.navi.hmi.setting.broadcast;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.setting.SettingCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.base.BaseModel;

public class SettingBroadcastModel extends BaseModel<SettingBroadcastViewModel> implements SettingCallback {


    private static final String TAG = SettingBroadcastModel.class.getName();

    private final SettingPackage settingPackage;

    public SettingBroadcastModel() {
        settingPackage = SettingPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        settingPackage.registerCallBack("SettingBroadcastModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public void initView() {
        setCruiseBroadcastOpen();
        setCruiseBroadcastRoadConditionOpen();
        setCruiseBroadcastSafeOpen();
        setCruiseBroadcastCameraOpen();
        setGuideBroadcast();
    }


    public void setCruiseBroadcastOpen() {
        boolean isOpen = settingPackage.getCruiseBroadcastOpen();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_CRUISE_BROADCAST,isOpen);
    }

    public void setCruiseBroadcastRoadConditionOpen() {
        boolean isOpen = settingPackage.getConfigKeyRoadWarn();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS,isOpen);
    }

    public void setCruiseBroadcastSafeOpen() {
        boolean isOpen = settingPackage.getConfigKeySafeBroadcast();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER,isOpen);
    }

    public void setCruiseBroadcastCameraOpen() {
        boolean isOpen = settingPackage.getConfigKeyDriveWarn();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE,isOpen);
    }

    private void setGuideBroadcast() {
        int mode = settingPackage.getConfigKeyBroadcastMode();
        switch (mode) {
            case 1 :
                mViewModel.onNaviBroadcastChange(false, true, false);
                break;
            case 2 :
                mViewModel.onNaviBroadcastChange(true, false, false);
                break;
            case 3 :
                mViewModel.onNaviBroadcastChange(false, false, true);
                break;
        }
    }

    public void setConfigKeyBroadcastMode(int broadcastMode) {
        settingPackage.setConfigKeyBroadcastMode(broadcastMode);
    }

    public void setConfigKeyRoadWarn(boolean roadWarn) {
        settingPackage.setConfigKeyRoadWarn(roadWarn);
    }

    public void setConfigKeySafeBroadcast(boolean safeBroadcast) {
        settingPackage.setConfigKeySafeBroadcast(safeBroadcast);
    }

    public void setConfigKeyDriveWarn(boolean driveWarn) {
        settingPackage.setConfigKeyDriveWarn(driveWarn);
    }

    public void setCruiseBroadcastOpen(boolean isOpen) {
        settingPackage.setCruiseBroadcastOpen(isOpen);
    }

    @Override
    public void notify(int eventType, int exCode) {
        Logger.d(TAG, "notify: eventType = " + eventType + " exCode = " + exCode);
    }
}
