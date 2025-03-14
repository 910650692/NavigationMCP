package com.fy.navi.service.adapter.recorder;

public interface IRecorderApi {

    void initService();

    // 开始录制
    void startRecorder();
    // 停止录制
    void stopRecorder();
    // 开始回放
    void startPlayback();
    // 停止回放
    void stopPlayback();

    void registerCallBack(String key, RecorderAdapterCallback callBack);

}