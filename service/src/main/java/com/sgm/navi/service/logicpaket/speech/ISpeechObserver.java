package com.sgm.navi.service.logicpaket.speech;


/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface ISpeechObserver {

    void onVoiceSet(String voicePackage, int result, String voiceName, boolean isBoolean);

}