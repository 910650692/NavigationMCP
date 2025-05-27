package com.fy.navi.service.adapter.recorder;

import com.fy.navi.service.AdapterConfig;
import java.util.Objects;

public final class RecorderAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(RecorderAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "RecorderAdapterImpl";
    private final IRecorderApi mRecorderApi;

    private RecorderAdapter() {
        mRecorderApi = (IRecorderApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    /**
     * 初始化服务
     */
    public void initService() {
        mRecorderApi.initService();
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param callBack 回调
     */
    public void registerCallBack(final String key, final RecorderAdapterCallback callBack) {
        mRecorderApi.registerCallBack(key, callBack);
    }

    /**
     * 开始记录
     */
    public void startRecorder() {
        mRecorderApi.startRecorder();
    }

    /**
     * 停止记录
     */
    public void stopRecorder() {
        mRecorderApi.stopRecorder();
    }

    /**
     * 是否正在录制
     */
    public boolean isRecording() {
        return mRecorderApi.isRecording();
    }

    /**
     * 开始回放
     */
    public void startPlayback() {
        mRecorderApi.startPlayback();
    }

    /**
     * 停止回放
     */
    public void stopPlayback() {
        mRecorderApi.stopPlayback();
    }

    /**
     * 是否正在播放
     */
    public boolean isPlaying() {
       return mRecorderApi.isPlaying();
    }


    public static RecorderAdapter getInstance() {
        return RecorderAdapter.Helper.RA;
    }
    private static final class Helper {
        private static final RecorderAdapter RA = new RecorderAdapter();
    }

}