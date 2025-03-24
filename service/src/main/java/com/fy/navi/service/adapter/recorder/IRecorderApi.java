package com.fy.navi.service.adapter.recorder;

public interface IRecorderApi {

    /**
     * 初始化服务
     */
    void initService();

    /**
     * 开始记录
     */
    void startRecorder();

    /**
     * 停止记录
     */
    void stopRecorder();

    /**
     * 开始回放
     */
    void startPlayback();

    /**
     * 停止回放
     */
    void stopPlayback();

    /**
     * 注册回调
     * @param key 回调key
     * @param callBack 回调
     */
    void registerCallBack(String key, RecorderAdapterCallback callBack);

}