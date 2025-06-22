package com.sgm.navi.hmi.setting.broadcast.voice;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.define.voice.VoiceInfo;
import com.sgm.navi.service.logicpaket.speech.SpeechPackage;
import com.sgm.navi.service.logicpaket.voice.VoiceCallback;
import com.sgm.navi.service.logicpaket.voice.VoicePackage;
import com.sgm.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.HashMap;

public class SettingVoiceBroadcastModel extends BaseModel<SettingVoiceBroadcastViewModel> implements VoiceCallback {
    private static final String TAG = SettingVoiceBroadcastModel.class.getSimpleName();

    private final VoicePackage mVoicePackage;
    private final SpeechPackage mSpeechPackage;

    public SettingVoiceBroadcastModel() {
        mVoicePackage = VoicePackage.getInstance();
        mSpeechPackage = SpeechPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mVoicePackage.registerCallBack(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 载操作
     * @param opType
     * @param voiceIdDiyLst
     * @return 返回int值
     */
    public int operate(final int opType, final ArrayList<Integer> voiceIdDiyLst) {
        return mVoicePackage.operate(opType, voiceIdDiyLst);
    }

    /**
     * 设置声音
     * @param irfPath
     */
    public void setVoice(final String irfPath, final String text) {
        mSpeechPackage.setVoice(irfPath);
        mSpeechPackage.synthesize(text);
    }

    @Override
    public void onDownLoadStatus(final int downLoadMode, final int dataType, final int id, final int taskCode, final int opCode) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(TAG, "onDownLoadStatus: downLoadMode = " + downLoadMode + "; dataType =  "
                    + dataType + "; id =  " + id + " " + taskCode + " " + opCode);
            mViewModel.onDownLoadStatus(downLoadMode, dataType, id, taskCode, opCode);
        });
    }

    @Override
    public void onPercent(final int downLoadMode, final int dataType, final int id, final int percentType, final float percent) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(TAG, "onPercent: downLoadMode = "  + downLoadMode +  " dataType = " +
                    dataType + " id = " + id + " percentType = " + percentType + " percent = " + percent);
            mViewModel.onPercent(downLoadMode, dataType, id, percentType, percent);
        });
    }

    @Override
    public void onOperated(final int downLoadMode, final int dataType, final int opType,
                           final ArrayList<Integer> opreatedIdList) {
        Logger.d(TAG,"onOperated: downLoadMode = "  + downLoadMode +  " dataType = " +
                dataType + " opType = " + opType + " opreatedIdList = " + opreatedIdList);
    }

    public HashMap<Integer, VoiceInfo> getRecommendVoiceList(){
        return mVoicePackage.getRecommendVoiceList();
    }
}
