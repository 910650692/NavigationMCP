package com.sgm.navi.service.adapter.speech;


public interface ISpeechAdapterCallback {

    void onVoiceSet(String irfPath, int result, String voicePackage, String voiceName, String voiceIcon, boolean isBoolean);

}
