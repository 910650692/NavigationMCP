package com.sgm.navi.service.adapter.voice;


import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.define.voice.VoiceInfo;

import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;

public class VoiceAdapter {

    private static final String SETTING_API_PKG = Objects.requireNonNull(VoiceAdapter.class.getPackage()).getName();
    private static final String SETTING_API_CLS = "VoiceAdapterImpl";
    private final VoiceApi mVoiceApi;

    private VoiceAdapter() {
        mVoiceApi = (VoiceApi) AdapterConfig.getObject(SETTING_API_PKG , SETTING_API_CLS);
    }

    public void initService() {
        mVoiceApi.initService();
    }

    public void unInitService() {
        mVoiceApi.unInitService();
    }

    public int isInitService() {
        return mVoiceApi.isInitService();
    }

    public void registerCallback(String key, VoiceAdapterCallback resultCallback) {
        mVoiceApi.registerCallback(key, resultCallback);
    }

    public int requestDataListCheck(int downLoadMode, String path) {
        return mVoiceApi.requestDataListCheck(downLoadMode, path);
    }

    public int abortRequestDataListCheck(int downLoadMode) {
        return mVoiceApi.abortRequestDataListCheck(downLoadMode);
    }

    public ArrayList<Integer> getVoiceIdList(int downLoadMode) {
        return mVoiceApi.getVoiceIdList(downLoadMode);
    }

    public ArrayList<Integer> getVoiceIdList(int downLoadMode, int engineType) {
        return mVoiceApi.getVoiceIdList(downLoadMode, engineType);
    }

    public Map<Integer, VoiceInfo> getRecommendVoiceList(){
        return mVoiceApi.getRecommendVoiceList();
    }

    public VoiceInfo getVoice(int	downloadMode, int voiceId) {
        return mVoiceApi.getVoice(downloadMode, voiceId);
    }

    public int requestDataImage(int downloadMode, int voiceId) {
        return mVoiceApi.requestDataImage(downloadMode, voiceId);
    }

    public int abortRequestDataImage(int downloadMode, int voiceId) {
        return mVoiceApi.abortRequestDataImage(downloadMode, voiceId);
    }

    public int operate(int opType, ArrayList<Integer> voiceIdDiyLst) {
        return mVoiceApi.operate(opType, voiceIdDiyLst);
    }

    public static VoiceAdapter getInstance() {
        return SingletonHolder.sInstance;
    }

    private static final class SingletonHolder {
        private static final VoiceAdapter sInstance = new VoiceAdapter();
    }
}
