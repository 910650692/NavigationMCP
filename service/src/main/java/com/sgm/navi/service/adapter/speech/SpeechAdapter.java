package com.sgm.navi.service.adapter.speech;

import com.sgm.navi.service.AdapterConfig;

import java.util.Objects;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class SpeechAdapter {

    private ISpeechApi mSpeechApi;

    private static class SpeechAdapterHolder {
        private static final SpeechAdapter mInstance = new SpeechAdapter();
    }

    public static SpeechAdapter getInstance() {
        return SpeechAdapterHolder.mInstance;
    }

    private SpeechAdapter() {
        mSpeechApi = (ISpeechApi) AdapterConfig.getObject(Objects.requireNonNull(this.getClass().getPackage()).getName(), "SpeechAdapterImpl");
    }

    public void init() {
        if (mSpeechApi != null) {
            mSpeechApi.init();
        }
    }

    public void unInit() {
        if (mSpeechApi != null) {
            mSpeechApi.unInit();
        }
    }

    public void registerCallback(ISpeechAdapterCallback callback) {
        if (mSpeechApi != null) {
            mSpeechApi.registerCallback(callback);
        }
    }

    public void unregisterCallback(ISpeechAdapterCallback callback) {
        if (mSpeechApi != null) {
            mSpeechApi.unregisterCallback(callback);
        }
    }

    public void setVoice(final String irfPath, final String voicePackage, final String voiceName,
                         final String voiceIcon, final boolean isBoolean) {
        if (mSpeechApi != null) {
            mSpeechApi.setVoice(irfPath, voicePackage, voiceName, voiceIcon, isBoolean);
        }
    }

    public void synthesize(boolean isNormalTTS, String text) {
        if (mSpeechApi != null) {
            mSpeechApi.synthesize(isNormalTTS, text);
        }
    }

    public void synthesize(String text) {
        if (mSpeechApi != null) {
            mSpeechApi.synthesize(true, text);
        }
    }

    public void synthesizeLast(String text) {
        if (mSpeechApi != null) {
            mSpeechApi.synthesize(text);
        }
    }

    public void stop() {
        if (mSpeechApi != null) {
            mSpeechApi.stop();
        }
    }
}
