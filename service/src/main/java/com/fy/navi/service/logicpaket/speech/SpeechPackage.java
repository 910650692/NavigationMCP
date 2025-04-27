package com.fy.navi.service.logicpaket.speech;


import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.speech.ISpeechAdapterCallback;
import com.fy.navi.service.adapter.speech.SpeechAdapter;

import java.util.concurrent.ConcurrentHashMap;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class SpeechPackage implements ISpeechAdapterCallback {
    private static final String TAG = MapDefaultFinalTag.SPEECH_SERVICE_TAG;
    private SpeechAdapter mSpeechAdapter;
    private ConcurrentHashMap<String, ISpeechObserver> mObserverMap = new ConcurrentHashMap<>();

    private SpeechPackage() {
        mSpeechAdapter = SpeechAdapter.getInstance();
    }

    public void unInit() {
        if (mSpeechAdapter != null) {
            mSpeechAdapter.unInit();
        }
    }

    public void init() {
        if (mSpeechAdapter != null) {
            mSpeechAdapter.init();
            mSpeechAdapter.registerCallback(this);
        }
    }

    public void addObserver(String observerKey, ISpeechObserver observer) {
        if (observer == null || observerKey == null) {
            Logger.e(TAG, "Failed to register callback: callback or identifier is null.");
            return;
        }
        if (!mObserverMap.containsKey(observerKey)) {
            mObserverMap.put(observerKey, observer);
        } else {
            Logger.w(TAG, "Callback with identifier {} already registered.", observerKey);
        }
    }

    public void removeObserver(String observerKey) {
        if (observerKey == null) {
            Logger.e(TAG, "Failed to unregister callback: identifier is null.");
            return;
        }
        mObserverMap.remove(observerKey);
    }

    /*设置语音包路径*/
    public void setVoice(String irfPath) {
        if (mSpeechAdapter != null) {
            mSpeechAdapter.setVoice(irfPath);
        }
    }

    /*播报文本*/
    public void synthesize(String text) {
        if (mSpeechAdapter != null) {
            mSpeechAdapter.synthesize(text);
        }
    }

    /*播报文本*/
    public void synthesize(boolean isNormalTTS, String text) {
        if (mSpeechAdapter != null) {
            mSpeechAdapter.synthesize(isNormalTTS, text);
        }
    }

    public void stop() {
        if (mSpeechAdapter != null) {
            mSpeechAdapter.stop();
        }
    }

    @Override
    public void onVoiceSet(int result) {
        for (ISpeechObserver observer : mObserverMap.values()) {
            observer.onVoiceSet(result);
        }
    }

    public static SpeechPackage getInstance() {
        return Helper.ep;
    }

    private static final class Helper {
        private static final SpeechPackage ep = new SpeechPackage();
    }
}
