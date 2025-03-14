package com.fy.navi.service.adapter.recorder;

import com.fy.navi.service.AdapterConfig;
import java.util.Objects;

public class RecorderAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(RecorderAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "RecorderAdapterImpl";
    private IRecorderApi mRecorderApi;

    private RecorderAdapter() {
        mRecorderApi = (IRecorderApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initService() {
        mRecorderApi.initService();
    }

    public void registerCallBack(String key, RecorderAdapterCallback callBack) {
        mRecorderApi.registerCallBack(key, callBack);
    }

    public void startRecorder() {
        mRecorderApi.startRecorder();
    }

    public void stopRecorder() {
        mRecorderApi.stopRecorder();
    }

    public void startPlayback() {
        mRecorderApi.startPlayback();
    }

    public void stopPlayback() {
        mRecorderApi.stopPlayback();
    }

    public static RecorderAdapter getInstance() {
        return RecorderAdapter.Helper.ra;
    }
    private static final class Helper {
        private static final RecorderAdapter ra = new RecorderAdapter();
    }

}