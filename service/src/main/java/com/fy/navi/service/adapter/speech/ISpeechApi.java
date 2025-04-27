package com.fy.navi.service.adapter.speech;


/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public interface ISpeechApi {
    void init();

    void unInit();

    void registerCallback(ISpeechAdapterCallback callback);

    void unregisterCallback(ISpeechAdapterCallback callback);

    int setVoice(String irfPath);

    void synthesize(boolean isNormalTTS, String text);

    void stop();
}
