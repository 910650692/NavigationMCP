package com.fy.navi.hmi.setting.broadcast;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.base.BaseModel;

public class SettingBroadcastModel extends BaseModel<SettingBroadcastViewModel> implements SettingPackage.SettingChangeCallback {


    private static final String TAG = SettingBroadcastModel.class.getName();

    private final SettingPackage mSettingPackage;

    public SettingBroadcastModel() {
        mSettingPackage = SettingPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mSettingPackage.setSettingChangeCallback("SettingBroadcastModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 初始化
     */
    public void initView() {
        setCruiseBroadcastOpen();
        setCruiseBroadcastRoadConditionOpen();
        setCruiseBroadcastSafeOpen();
        setCruiseBroadcastCameraOpen();
        setGuideBroadcast();
    }

    /**
     *  设置巡航播报开关
     */
    public void setCruiseBroadcastOpen() {
        final boolean isOpen = mSettingPackage.getCruiseBroadcastOpen();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_CRUISE_BROADCAST,isOpen);
    }

    /**
     * 设置巡航播报前方路况
     */
    public void setCruiseBroadcastRoadConditionOpen() {
        final boolean isOpen = mSettingPackage.getConfigKeyRoadWarn();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS,isOpen);
    }

    /**
     * 设置巡航播报安全提醒
     */
    public void setCruiseBroadcastSafeOpen() {
        final boolean isOpen = mSettingPackage.getConfigKeySafeBroadcast();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER,isOpen);
    }

    /**
     * 设置巡航播报电子眼播报
     */
    public void setCruiseBroadcastCameraOpen() {
        final boolean isOpen = mSettingPackage.getConfigKeyDriveWarn();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE,isOpen);
    }

    /**
     * 设置导航播报模式
     */
    private void setGuideBroadcast() {
        final int mode = mSettingPackage.getConfigKeyBroadcastMode();
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
            default:
                Logger.d(TAG,"Invalid value, mode = " + mode);
                break;
        }
    }

    /**
     * 设置导航播报模式
     * @param broadcastMode
     */
    public void setConfigKeyBroadcastMode(final int broadcastMode) {
        mSettingPackage.setConfigKeyBroadcastMode(broadcastMode);
    }

    /**
     * 设置巡航播报前方路况
     * @param roadWarn
     */
    public void setConfigKeyRoadWarn(final boolean roadWarn) {
        mSettingPackage.setConfigKeyRoadWarn(roadWarn);
    }

    /**
     * 设置巡航播报电子眼播报
     * @param safeBroadcast
     */
    public void setConfigKeySafeBroadcast(final boolean safeBroadcast) {
        mSettingPackage.setConfigKeySafeBroadcast(safeBroadcast);
    }

    /**
     * 设置巡航播报安全提醒
     * @param driveWarn
     */
    public void setConfigKeyDriveWarn(final boolean driveWarn) {
        mSettingPackage.setConfigKeyDriveWarn(driveWarn);
    }

    /**
     * 设置巡航播报开关
     * @param isOpen
     */
    public void setCruiseBroadcastOpen(final boolean isOpen) {
        mSettingPackage.setCruiseBroadcastOpen(isOpen);
    }

    @Override
    public void onSettingChanged(final String key, final String value) {
        Logger.d(TAG,"onSettingChanged, key = " + key + ", value = " + value);
        if (key.equals(SettingController.KEY_SETTING_NAVI_BROADCAST)) {
            switch (value) {
                case SettingController.VALUE_NAVI_BROADCAST_DETAIL:
                    ThreadManager.getInstance().postUi(() -> mViewModel.onNaviBroadcastChange(true, false, false));
                    break;
                case SettingController.VALUE_NAVI_BROADCAST_CONCISE:
                    ThreadManager.getInstance().postUi(() -> mViewModel.onNaviBroadcastChange(false, true, false));
                    break;
                case SettingController.VALUE_NAVI_BROADCAST_SIMPLE:
                    ThreadManager.getInstance().postUi(() -> mViewModel.onNaviBroadcastChange(false, false, true));
                    break;
                default:
                    break;
            }
        }
    }
}
