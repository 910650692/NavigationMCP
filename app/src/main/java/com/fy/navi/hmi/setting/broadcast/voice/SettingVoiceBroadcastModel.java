package com.fy.navi.hmi.setting.broadcast.voice;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.voice.VoiceInfo;
import com.fy.navi.service.logicpaket.speech.SpeechPackage;
import com.fy.navi.service.logicpaket.voice.VoiceCallback;
import com.fy.navi.service.logicpaket.voice.VoicePackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.HashMap;

public class SettingVoiceBroadcastModel extends BaseModel<SettingVoiceBroadcastViewModel> implements VoiceCallback {
    private static final String TAG = SettingVoiceBroadcastModel.class.getSimpleName();

    private final VoicePackage voicePackage;
    private final SpeechPackage speechPackage;

    public SettingVoiceBroadcastModel() {
        voicePackage = VoicePackage.getInstance();
        speechPackage = SpeechPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        voicePackage.registerCallBack(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public int isInitService() {
        return voicePackage.isInitService();
    }

    public void requestDataListCheck(int downLoadMode, String path) {
        voicePackage.requestDataListCheck(downLoadMode, path);
    }

    public ArrayList<Integer> getVoiceIdList(int downLoadMode) {
        return voicePackage.getVoiceIdList(downLoadMode);
    }

    public ArrayList<Integer> getVoiceIdList(int downLoadMode, int engineType) {
        return voicePackage.getVoiceIdList(downLoadMode, engineType);
    }

    public VoiceInfo getVoice(int	downloadMode, int voiceId) {
        return voicePackage.getVoice(downloadMode, voiceId);
    }

    public int requestDataImage(int downloadMode, int voiceId) {
        return voicePackage.requestDataImage(downloadMode, voiceId);
    }

    public int abortRequestDataImage(int downloadMode, int voiceId) {
        return voicePackage.abortRequestDataImage(downloadMode, voiceId);
    }

    public int abortRequestDataListCheck(int downLoadMode) {
        return voicePackage.abortRequestDataListCheck(downLoadMode);
    }

    public void operate(int downLoadMode, int opType, ArrayList<Integer> voiceIdDiyLst) {
        voicePackage.operate(downLoadMode, opType, voiceIdDiyLst);
    }

    public void setVoice(String irfPath) {
        speechPackage.setVoice(irfPath);
    }

    @Override
    public void onInit(int downLoadMode, int dataType, int opCode) {
    }

    @Override
    public void onDownloadImage(int itemId, int opErrCode, String strFilePath, int dataType) {

    }

    @Override
    public void onRequestDataListCheck(int downLoadMode, int dataType, int opCode) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(TAG, "onRequestDataListCheck: " + downLoadMode + " " + dataType + " " + opCode);
            mViewModel.onRequestDataListCheck(downLoadMode, dataType, opCode);
        });
    }

    @Override
    public void onDownLoadStatus(int downLoadMode, int dataType, int id, int taskCode, int opCode) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(TAG, "onDownLoadStatus: " + downLoadMode + " " + dataType + " " + id + " " + taskCode + " " + opCode);
            mViewModel.onDownLoadStatus(downLoadMode, dataType, id, taskCode, opCode);
        });
    }

    @Override
    public void onPercent(int downLoadMode, int dataType, int id, int percentType, float percent) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(TAG, "onPercent: downLoadMode = "  + downLoadMode +  " dataType = " + dataType + " id = " + id + " percentType = " + percentType + " percent = " + percent);
            mViewModel.onPercent(downLoadMode, dataType, id, percentType, percent);
        });
    }

    @Override
    public void onOperated(int downLoadMode, int dataType, int opType, ArrayList<Integer> opreatedIdList) {
        Logger.d(TAG, "onOperated: downLoadMode = "  + downLoadMode +  " dataType = " + dataType + " opType = " + opType + " opreatedIdList = " + opreatedIdList);
    }

    public HashMap<Integer, VoiceInfo> getRecommendVoiceList(){
        return voicePackage.getRecommendVoiceList();
    }
}
