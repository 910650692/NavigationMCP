package com.sgm.navi.hmi.setting.broadcast;

import android.text.TextUtils;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.define.voice.VoiceInfo;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.speech.ISpeechObserver;
import com.sgm.navi.service.logicpaket.speech.SpeechPackage;
import com.sgm.navi.service.logicpaket.voice.VoicePackage;
import com.sgm.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class SettingBroadcastModel extends BaseModel<SettingBroadcastViewModel> implements SettingPackage.SettingChangeCallback, ISpeechObserver {


    private static final String TAG = SettingBroadcastModel.class.getName();

    private final SettingPackage mSettingPackage;
    private final SpeechPackage mSpeechPackage;
    private final VoicePackage mVoicePackage;

    public SettingBroadcastModel() {
        mSpeechPackage = SpeechPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mVoicePackage = VoicePackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mSettingPackage.setSettingChangeCallback("SettingBroadcastModel",this);
        mSpeechPackage.addObserver("SettingBroadcastModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mSpeechPackage.removeObserver("SettingBroadcastModel");
        mSettingPackage.unRegisterSettingChangeCallback("SettingBroadcastModel");
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
        final boolean isOpen = mSettingPackage.getConfigKeyDriveWarn();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER,isOpen);
    }

    /**
     * 设置巡航播报电子眼播报
     */
    public void setCruiseBroadcastCameraOpen() {
        final boolean isOpen = mSettingPackage.getConfigKeySafeBroadcast();
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

    private String mBroadcastTypeTts = "";
    @Override
    public void onSettingChanged(final String key, final String value) {
        Logger.d(TAG, "onSettingChanged, key = " + key + ", value = " + value);
        String tts = "";
        if (key.equals(SettingController.KEY_SETTING_NAVI_BROADCAST)) {
            switch (value) {
                case SettingController.VALUE_NAVI_BROADCAST_DETAIL:
                    ThreadManager.getInstance().postUi(() -> mViewModel.onNaviBroadcastChange(true, false, false));
                    tts = String.format(ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_broadcast_switch),
                            ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_broadcast_detail));
                    break;
                case SettingController.VALUE_NAVI_BROADCAST_CONCISE:
                    ThreadManager.getInstance().postUi(() -> mViewModel.onNaviBroadcastChange(false, true, false));
                    tts = String.format(ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_broadcast_switch),
                            ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_broadcast_concise));
                    break;
                case SettingController.VALUE_NAVI_BROADCAST_SIMPLE:
                    ThreadManager.getInstance().postUi(() -> mViewModel.onNaviBroadcastChange(false, false, true));
                    tts = String.format(ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_broadcast_switch),
                            ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_broadcast_minimalism));
                    break;
                default:
                    break;
            }
        }
        if (!TextUtils.isEmpty(tts) && !TextUtils.equals(mBroadcastTypeTts, tts)) {
            SpeechPackage.getInstance().synthesizeLast(tts);
            mBroadcastTypeTts = tts;
        }
    }

    @Override
    public void onVoiceSet(int result) {
        mViewModel.setCurrentVoice();
    }

    public List<String> getRecommendVoiceList() {
        Map<Integer, VoiceInfo> voiceMap = mVoicePackage.getRecommendVoiceList();
        List<String> mVoiceIcon = new ArrayList<>();
        if(voiceMap != null && !voiceMap.isEmpty()){
            List<VoiceInfo> voiceList = new ArrayList<>(voiceMap.values());
            if(!voiceList.isEmpty()){
                int i = 0;
                do{
                    mVoiceIcon.add(i < voiceList.size() - 1 ? voiceList.get(i).getImageUrl() : "");
                    i++;
                }while (i < 4);
            }
        }
        return mVoiceIcon;
    }
}
