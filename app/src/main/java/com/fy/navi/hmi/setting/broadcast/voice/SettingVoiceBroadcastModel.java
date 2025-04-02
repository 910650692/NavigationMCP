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

    public int isInitService() {
        return mVoicePackage.isInitService();
    }

    /**
     * 检查数据列表
     * @param downLoadMode
     * @param path
     * @return 返回int值
     */
    public int requestDataListCheck(final int downLoadMode, final String path) {
        return mVoicePackage.requestDataListCheck(downLoadMode, path);
    }

    /**
     * 获取所有语音包id列表
     * @param downLoadMode
     * @return 语音包id列表
     */
    public ArrayList<Integer> getVoiceIdList(final int downLoadMode) {
        return mVoicePackage.getVoiceIdList(downLoadMode);
    }

    /**
     * 获取指定类型(讯飞/MIT)的语音包id列表
     * @param downLoadMode
     * @param engineType
     * @return 语音包id列表
     */
    public ArrayList<Integer> getVoiceIdList(final int downLoadMode, final int engineType) {
        return mVoicePackage.getVoiceIdList(downLoadMode, engineType);
    }

    /**
     * 根据语音包id获取语音信息
     * @param downloadMode
     * @param voiceId
     * @return 语音信息
     */
    public VoiceInfo getVoice(final int downloadMode, final int voiceId) {
        return mVoicePackage.getVoice(downloadMode, voiceId);
    }

    /**
     * 网络请求语音头像
     * @param downloadMode
     * @param voiceId
     * @return 返回错误码
     */
    public int requestDataImage(final int downloadMode, final int voiceId) {
        return mVoicePackage.requestDataImage(downloadMode, voiceId);
    }

    /**
     * 终止网络请求语音头像
     * @param downloadMode
     * @param voiceId
     * @return 返回错误码
     */
    public int abortRequestDataImage(final int downloadMode, final int voiceId) {
        return mVoicePackage.abortRequestDataImage(downloadMode, voiceId);
    }

    /**
     * 终止行政区域表单网络请求
     * @param downLoadMode
     * @return 返回int值
     */
    public int abortRequestDataListCheck(final int downLoadMode) {
        return mVoicePackage.abortRequestDataListCheck(downLoadMode);
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
    public void setVoice(final String irfPath) {
        mSpeechPackage.setVoice(irfPath);
    }

    @Override
    public void onInit(final int downLoadMode, final int dataType, final int opCode) {
    }

    @Override
    public void onDownloadImage(final int itemId, final int opErrCode, final String strFilePath, final int dataType) {

    }

    @Override
    public void onRequestDataListCheck(final int downLoadMode, final int dataType, final int opCode) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(TAG, "onRequestDataListCheck: downLoadMode = " + downLoadMode + "; dataType = " +
                    dataType + "; opCode = " + opCode);
            mViewModel.onRequestDataListCheck(downLoadMode, dataType, opCode);
        });
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
