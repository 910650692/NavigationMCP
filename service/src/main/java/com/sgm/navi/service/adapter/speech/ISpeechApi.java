package com.sgm.navi.service.adapter.speech;


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

    int setVoice(final String irfPath, final String voicePackage, final String voiceName,
                 final String voiceIcon, final boolean isBoolean);

    void synthesize(boolean isNormalTTS, String text);

    void synthesize(String text);

    void stop();
}
