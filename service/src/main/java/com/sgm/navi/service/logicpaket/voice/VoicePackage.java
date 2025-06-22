package com.sgm.navi.service.logicpaket.voice;

import com.sgm.navi.service.adapter.voice.VoiceAdapter;
import com.sgm.navi.service.adapter.voice.VoiceAdapterCallback;
import com.sgm.navi.service.define.voice.VoiceInfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class VoicePackage implements VoiceAdapterCallback {

    public static final String TAG = VoicePackage.class.getSimpleName();

    private final VoiceAdapter voiceAdapter;
    private final List<VoiceCallback> callBack = new ArrayList<>();

    private static final class SInstanceHolder {
        private static final VoicePackage instance = new VoicePackage();
    }

    public static VoicePackage getInstance() {
        return SInstanceHolder.instance;
    }

    private VoicePackage() {
        voiceAdapter = VoiceAdapter.getInstance();
    }

    public synchronized void registerCallBack(VoiceCallback callback) {
        if (callback != null && !callBack.contains(callback)) {
            callBack.add(callback);
        }
    }
    public void init() {
        voiceAdapter.initService();
        voiceAdapter.registerCallback("VoicePackage", this);
    }

    public void unInit() {
        voiceAdapter.unInitService();
    }

    public int isInitService() {
        return voiceAdapter.isInitService();
    }

    public int requestDataListCheck(int downLoadMode, String path) {
        return voiceAdapter.requestDataListCheck(downLoadMode, path);
    }

    public int abortRequestDataListCheck(int downLoadMode) {
        return voiceAdapter.abortRequestDataListCheck(downLoadMode);
    }

    public ArrayList<Integer> getVoiceIdList(int downLoadMode) {
        return voiceAdapter.getVoiceIdList(downLoadMode);
    }

    public ArrayList<Integer> getVoiceIdList(int downLoadMode, int engineType) {
        return voiceAdapter.getVoiceIdList(downLoadMode, engineType);
    }

    public VoiceInfo getVoice(int	downloadMode, int voiceId) {
        return voiceAdapter.getVoice(downloadMode, voiceId);
    }

    public int requestDataImage(int downloadMode, int voiceId) {
        return voiceAdapter.requestDataImage(downloadMode, voiceId);
    }

    public int abortRequestDataImage(int downloadMode, int voiceId) {
        return voiceAdapter.abortRequestDataImage(downloadMode, voiceId);
    }

    public int operate(int opType, ArrayList<Integer> voiceIdDiyLst) {
        return voiceAdapter.operate(opType, voiceIdDiyLst);
    }

    public HashMap<Integer, VoiceInfo> getRecommendVoiceList(){
        return voiceAdapter.getRecommendVoiceList();
    }

    @Override
    public void onInit(int downLoadMode, int dataType, int opCode) {
        for (VoiceCallback callback : callBack) {
            callback.onInit(downLoadMode, dataType, opCode);
        }
    }

    @Override
    public void onDownloadImage(int itemId, int opErrCode, String strFilePath, int dataType) {
        for (VoiceCallback callback : callBack) {
            callback.onDownloadImage(itemId, opErrCode, strFilePath, dataType);
        }
    }

    @Override
    public void onRequestDataListCheck(int downLoadMode, int dataType, int opCode) {
        for (VoiceCallback callback : callBack) {
            callback.onRequestDataListCheck(downLoadMode, dataType, opCode);
        }
    }

    @Override
    public void onDownLoadStatus(int downLoadMode, int dataType, int id, int taskCode, int opCode) {
        for (VoiceCallback callback : callBack) {
            callback.onDownLoadStatus(downLoadMode, dataType, id, taskCode, opCode);
        }
    }

    @Override
    public void onPercent(int downLoadMode, int dataType, int id, int percentType, float percent) {
        for (VoiceCallback callback : callBack) {
            callback.onPercent(downLoadMode, dataType, id, percentType, percent);
        }
    }

    @Override
    public void onOperated(int downLoadMode, int dataType, int opType, ArrayList<Integer> opreatedIdList) {
        for (VoiceCallback callback : callBack) {
            callback.onOperated(downLoadMode, dataType, opType, opreatedIdList);
        }
    }
}
