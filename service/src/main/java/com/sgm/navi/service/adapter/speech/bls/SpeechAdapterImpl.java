package com.sgm.navi.service.adapter.speech.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.speech.SpeechSynthesizeService;
import com.autonavi.gbl.speech.observer.ISpeechSynthesizeObserver;
import com.autonavi.gbl.util.model.BinaryStream;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.speech.ISpeechAdapterCallback;
import com.sgm.navi.service.adapter.speech.ISpeechApi;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.tts.NaviAudioPlayer;

import java.util.concurrent.CopyOnWriteArrayList;

/**
 * 导航播报实现类
 */
public class SpeechAdapterImpl implements ISpeechSynthesizeObserver, ISpeechApi {
    private static final String TAG = MapDefaultFinalTag.SPEECH_SERVICE_TAG;
    private int mInitResult;
    //SpeechSynthesizeService 是 AutoSDK 中用于语音合成服务的核心类，主要负责将文本转换为语音（TTS，Text To Speech）并提供相关功能支持
    private SpeechSynthesizeService mSpeechService;
    private final CopyOnWriteArrayList<ISpeechAdapterCallback> mSpeechAdapterCallback;
    //taskId 用于标识每一个语音合成任务，每一个任务的taskId都是唯一的
    private int taskId = 0;
    private int mSampleRate = 0;
    public int playingRequestId = 0;
    private boolean mIsNormalTTS = true;

    public SpeechAdapterImpl() {
        mSpeechService = (SpeechSynthesizeService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.SpeechSynthesizeSingleServiceID);
        mSpeechAdapterCallback = new CopyOnWriteArrayList<>();
    }

    @Override
    public void init() {
        mInitResult = mSpeechService.init(this);
        String path = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_VOICE_PATH);
        if (mInitResult == 0 && path != null && !path.isEmpty()) {
            int voiceResult = setVoice(path);
            Logger.i(TAG, "mInitResult：" + mInitResult + ",voiceResult：" + voiceResult);
        }
    }

    @Override
    public void unInit() {
        if (mSpeechService != null) {
            mSpeechService.unInit();
        }
    }

    @Override
    public void registerCallback(ISpeechAdapterCallback callback) {
        if (!mSpeechAdapterCallback.contains(callback)) {
            mSpeechAdapterCallback.add(callback);
        }
    }

    @Override
    public void unregisterCallback(ISpeechAdapterCallback callback) {
        mSpeechAdapterCallback.remove(callback);
    }

    /**
     * 切换语音包（默认语音/明星语音）
     *
     * @param irfPath 语音包路劲
     * @return 返回码，是否设置成功 errorcode.common.Service.ErrorCodeOK：表示设置成功。
     */
    @Override
    public int setVoice(String irfPath) {
        int result = mSpeechService.setVoice(irfPath);
        Logger.i(TAG, "result：" + result);

        for (ISpeechAdapterCallback callback : mSpeechAdapterCallback) {
            callback.onVoiceSet(result);
        }

        return result;
    }

    @Override
    public void synthesize(boolean isNormalTTS, String text) {
        Logger.i(TAG, "text：" + text + ",isNormalTTS：" + isNormalTTS);
        if (!ConvertUtils.isEmpty(text)) {
            if (null != mSpeechService) {
                mIsNormalTTS = isNormalTTS;
                playingRequestId = taskId;
                mSpeechService.synthesize(text, false, taskId++);
            }
        }
    }

    /**
     * 当语音合成被手动停止时触发的回调
     */
    @Override
    public void stop() {
        Logger.i(TAG, "stop");
        if (null != mSpeechService) {
            mSpeechService.stop(taskId - 1);
        }
    }

    /**
     * @param sampleRate sampleRate 采样率
     * @param pcmLen     PCM分片大小
     */
    @Override
    public void onSampleRateChange(int sampleRate, int[] pcmLen) {
        Logger.d(TAG, "onSampleRateChange sampleRate:" + sampleRate);
        mSampleRate = sampleRate;
    }

    /**
     * 当语音合成开始时触发的回调
     *
     * @param requestId requestId 请求唯一标识符
     */
    @Override
    public void onStart(int requestId) {
        Logger.d(TAG, "onStart requestId:" + requestId);
        NaviAudioPlayer.getInstance().createAudioTrack(requestId, mSampleRate, mIsNormalTTS);
    }

    /**
     * 返回语音合成的数据，可能会分多次回调
     *
     * @param requestId requestId 请求唯一标识符
     * @param pcmData   pcmData 返回的音频数据
     * @param duration  duration 返回的音频数据长度，单位是毫秒
     */
    @Override
    public void onGetData(int requestId, BinaryStream pcmData, long duration) {
        Logger.i(TAG, "requestId:" + requestId, "duration:" + duration);
        NaviAudioPlayer.getInstance().playPcmData(requestId, pcmData.buffer, mSampleRate);
    }

    /**
     * 当语音合成过程中发生错误时触发的回调
     *
     * @param requestId requestId 请求唯一标识符
     * @param errCode   errCode 错误码
     */
    @Override
    public void onError(int requestId, int errCode) {
        Logger.d(TAG, "onError requestId:" + requestId, "errCode:" + errCode);
        NaviAudioPlayer.getInstance().releaseAudioTrack(requestId);
    }

    /**
     * 当语音合成正常结束时触发的回调
     *
     * @param requestId requestId 请求唯一标识符
     */
    @Override
    public void onFinish(int requestId) {
        Logger.d(TAG, "onFinish requestId:" + requestId);
        NaviAudioPlayer.getInstance().releaseAudioTrack(requestId);
    }

    @Override
    public void onStop(int requestId) {
        Logger.d(TAG, "onStop requestId:" + requestId);
        NaviAudioPlayer.getInstance().releaseAudioTrack(requestId);
    }
}
