package com.sgm.navi.l2pp;

import com.android.utils.log.Logger;
import com.sgm.navi.service.define.navi.SoundInfoEntity;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.tts.TTSPlayHelper;

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
        Logger.i(TAG, "sendTTS: tts = " , tts , ", highPriority = " , highPriority);
        final SoundInfoEntity info = new SoundInfoEntity();
        info.setText(tts);
        info.setRangeType(TTSPlayHelper.RANGETYPE_NAVIGATION_START());
        info.setHighPriority(highPriority);
        NaviPackage.getInstance().onPlayTTS(info);
    }
}
