package com.fy.navi.l2pp;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.navi.PlayModule;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.logicpaket.navi.NaviPackage;

public class L2NopTts {
    private static final String TAG = L2NopTts.class.getSimpleName();

    /**
     * 发送语音
     *
     * @param tts
     */
    public static void sendTTS(final String tts) {
        sendTTS(tts, false);
    }

    /**
     * 发送语音
     *
     * @param tts
     * @param highPriority
     */
    public static void sendTTS(final String tts, final boolean highPriority) {
        Logger.i(TAG, "sendTTS: tts = " + tts + ", highPriority = " + highPriority);
        final SoundInfoEntity info = new SoundInfoEntity();
        info.setText(tts);
        info.setSoundType(PlayModule.PlayModuleLaneNavi);
        info.setHighPriority(highPriority);
        NaviPackage.getInstance().onPlayTTS(info);
    }
}
