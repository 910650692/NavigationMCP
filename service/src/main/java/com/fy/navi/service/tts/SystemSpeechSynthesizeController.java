package com.fy.navi.service.tts;

import android.content.Context;
import android.media.AudioAttributes;
import android.media.AudioFocusRequest;
import android.media.AudioFormat;
import android.media.AudioManager;
import android.media.AudioTrack;
import android.os.Build;
import android.os.Bundle;
import android.os.SystemClock;
import android.speech.tts.TextToSpeech;
import android.speech.tts.UtteranceProgressListener;

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

public class SystemSpeechSynthesizeController extends UtteranceProgressListener implements TextToSpeech.OnInitListener{
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private TextToSpeech mTextToSpeech;
    private AudioManager mAudioManager;
    private int mVolumPercent = 20; // 0~100
    private static final String UTTERANCEID = "T2SPId_";
    private String autoUtteranceId = UTTERANCEID + SystemClock.elapsedRealtime();
    private AudioFocusRequest mNaviFocusRequest;
    private AudioAttributes mNaviPlaybackAttributes;
    private int mGain;
    private final AudioManager.OnAudioFocusChangeListener onNaviAudioFocusChangeListener = new AudioManager.OnAudioFocusChangeListener() {
        @Override
        public void onAudioFocusChange(final int focusChange) {
            mGain = focusChange;
            Logger.d(TAG, " onAudioFocusChange " + focusChange);
            if (focusChange == AudioManager.AUDIOFOCUS_LOSS || focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT) {
                stop();
            }
        }
    };

    public void init() {
        Logger.d(TAG, "init() start");
        mTextToSpeech = new TextToSpeech(AppContext.mApplication.getApplicationContext(), this);
        List<TextToSpeech.EngineInfo> engines = mTextToSpeech.getEngines();
        for (TextToSpeech.EngineInfo engineInfo : engines) {
            Logger.i(TAG, "engineInfo " + engineInfo);
        }
        mAudioManager = (AudioManager) AppContext.mApplication.getApplicationContext().getSystemService(Context.AUDIO_SERVICE);
        mNaviPlaybackAttributes = new AudioAttributes.Builder()
                .setUsage(AudioAttributes.USAGE_ASSISTANCE_NAVIGATION_GUIDANCE)
                .setContentType(AudioAttributes.CONTENT_TYPE_SPEECH)
                .build();
        mNaviFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN_TRANSIENT_MAY_DUCK)
                .setAudioAttributes(mNaviPlaybackAttributes)
                .setAcceptsDelayedFocusGain(false)
                .setOnAudioFocusChangeListener(onNaviAudioFocusChangeListener)
                .build();
        Logger.d(TAG, "init() end");
        NaviAudioPlayer.getInstance().init();
    }

    @Override
    public void onInit(int status) {
        Logger.d(TAG, " onInit status = " + status);
        if (status != TextToSpeech.SUCCESS) {
            Logger.e(TAG, "init error");
            return;
        }
        int result = mTextToSpeech.setLanguage(Locale.getDefault());
        if (result == TextToSpeech.LANG_MISSING_DATA || result == TextToSpeech.LANG_NOT_SUPPORTED) {
            Logger.e(TAG, "Language not supported");
        }
        mTextToSpeech.setPitch(1.0f);
        mTextToSpeech.setSpeechRate(1.0f);
        AudioAttributes.Builder attrBuilder = getAudioAttributesBuilder();
        mTextToSpeech.setAudioAttributes(attrBuilder.build());
        mTextToSpeech.setOnUtteranceProgressListener(this);
    }

    public void release() {
        Logger.d(TAG, "AutoTTSPlayer release()");
        if (mTextToSpeech != null) {
            mTextToSpeech.stop();
            mTextToSpeech.shutdown();
            mTextToSpeech = null;
        }
        abandonNaviAudioFocus();
    }

    private boolean requestNaviAudioFocus() {
        if (AudioManager.AUDIOFOCUS_REQUEST_GRANTED == mGain) {
            return true;
        }
        Logger.d(TAG, "requestNaviAudioFocus");
        mGain = mAudioManager.requestAudioFocus(mNaviFocusRequest);
        Logger.d(TAG, "onAudioFocusChange:" + mGain);
        return AudioManager.AUDIOFOCUS_REQUEST_GRANTED == mGain;
    }

    public synchronized void abandonNaviAudioFocus() {
        if (mGain == AudioManager.AUDIOFOCUS_LOSS) {
            return;
        }
        if (mAudioManager != null && mNaviFocusRequest != null) {
            mAudioManager.abandonAudioFocusRequest(mNaviFocusRequest);
        }
        mGain = AudioManager.AUDIOFOCUS_LOSS;
    }

    public void playText(String text) {
        if (ConvertUtils.isEmpty(text)) {
            return;
        }
        playText(text, mVolumPercent);
    }

    public void playText(String text, int volumPercent) {
        Logger.d(TAG, "playText, text: " + text + ",volumPercent：" + volumPercent
                + ",autoUtteranceId：" + autoUtteranceId + ",mGain：" + mGain);
        if (!requestNaviAudioFocus()) {
            return;
        }
        autoUtteranceId = UTTERANCEID + SystemClock.elapsedRealtime();
        if (mTextToSpeech == null || ConvertUtils.isEmpty(text)) {
            return;
        }
        HashMap<String, String> textToSpeechParams = new HashMap<>();
        textToSpeechParams.put(TextToSpeech.Engine.KEY_PARAM_UTTERANCE_ID, autoUtteranceId);
        mTextToSpeech.speak(text, TextToSpeech.QUEUE_ADD, null, autoUtteranceId);
//        String appFilePath = FileUtils.APP_FILE_PATH + "/tts_output.wav";
//        File outputFile = new File(appFilePath);
//        Bundle bundle = new Bundle();
//        mTextToSpeech.synthesizeToFile(text, bundle, outputFile, "utteranceId");
    }

    public void setVolume(int value) {
        mVolumPercent = value;
    }

    public void stop() {
        Logger.d(TAG, "stop()");
        if (mTextToSpeech != null) {
            mTextToSpeech.stop();
        }
    }

    public boolean isPlaying() {
        if (mTextToSpeech != null) {
            return mTextToSpeech.isSpeaking();
        }
        return false;
    }

    private AudioAttributes.Builder getAudioAttributesBuilder() {
        AudioAttributes.Builder attrBuilder = new AudioAttributes.Builder();
        attrBuilder.setContentType(AudioAttributes.CONTENT_TYPE_SPEECH);
        attrBuilder.setUsage(AudioAttributes.USAGE_ASSISTANCE_NAVIGATION_GUIDANCE);
        return attrBuilder;
    }


    public void playNaviWarningSound(int type) {
        Logger.d(TAG, "AutoTTSPlayer playNaviWarningSound, type={?}", type);
    }

    public void playArSound(int type) {
        Logger.d(TAG, "AutoTTSPlayer playARSound type={?}", type);
    }

    public void playGroupSound(int type) {
        Logger.d(TAG, "AutoTTSPlayer playGroupSound type={?}", type);
    }

    @Override
    public void onStart(String utteranceId) {
        Logger.d(TAG, "开始生成音频数据：", utteranceId);
        NaviAudioPlayer.getInstance().createAudioTrack(11111, 44100);
    }

    @Override
    public void onDone(String utteranceId) {
        Logger.d(TAG, "完成音频数据生成：", utteranceId);
    }

    @Override
    public void onError(String utteranceId) {
        Logger.d(TAG, "生成音频数据出错 ", utteranceId);
    }

    @Override
    public void onAudioAvailable(String utteranceId, byte[] audio) {
//        Logger.d(TAG, "实时获取音频数据并传递给 AudioTrack：", utteranceId);
//        NaviAudioPlayer.getInstance().playPcmData(11111, audio, 44100);
    }
}
