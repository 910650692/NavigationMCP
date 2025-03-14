package com.fy.navi.service.logicpaket.speech;


import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.speech.ISpeechAdapterCallback;
import com.fy.navi.service.adapter.speech.SpeechAdapter;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class SpeechPackage {
    private static final String TAG = MapDefaultFinalTag.SPEECH_SERVICE_TAG;
    private SpeechAdapter mSpeechAdapter;

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
        }
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

    public void stop() {
        if (mSpeechAdapter != null) {
            mSpeechAdapter.stop();
        }
    }


    public static SpeechPackage getInstance() {
        return Helper.ep;
    }

    private static final class Helper {
        private static final SpeechPackage ep = new SpeechPackage();
    }
}
