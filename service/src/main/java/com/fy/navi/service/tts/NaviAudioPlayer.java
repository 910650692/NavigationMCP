package com.fy.navi.service.tts;

import android.content.Context;
import android.media.AudioAttributes;
import android.media.AudioFocusRequest;
import android.media.AudioFormat;
import android.media.AudioManager;
import android.media.AudioRecordingConfiguration;
import android.media.AudioTrack;
import android.media.MediaRecorder;
import android.media.VolumeShaper;
import android.os.CountDownTimer;
import android.util.SparseArray;

import androidx.lifecycle.MutableLiveData;

import com.android.utils.log.Logger;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;

import java.lang.reflect.Field;
import java.util.List;

public class NaviAudioPlayer {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private final MutableLiveData<Boolean> mISSDKTTSPlaying = new MutableLiveData<>();
    // 音量变化配置（比如快速淡入淡出）
    VolumeShaper.Configuration configurationFadeFast = new VolumeShaper.Configuration.Builder()
            .setDuration(50)
            .setCurve(new float[]{0.f, 0.2f, 0.4f, 0.6f, 0.8f, 1.0f}, new float[]{0.f, 0.049f, 0.134f, 0.247f, 0.558f, 1.0f})
            .setInterpolatorType(VolumeShaper.Configuration.INTERPOLATOR_TYPE_LINEAR)
            .build();
    private int mGain;
    private AudioManager mAudioManager;
    private AudioFocusRequest mMediaFocusRequest;
    private AudioAttributes mMediaAttributes;
    private AudioFocusRequest mNaviFocusRequest;
    private AudioAttributes mNaviPlaybackAttributes;
    private VolumeShaper mVolumeShaper = null;
    private boolean mIsNormalTTS = true;
    private final AudioManager.OnAudioFocusChangeListener onMediaAudioFocusChangeListener = new AudioManager.OnAudioFocusChangeListener() {
        @Override
        public void onAudioFocusChange(final int focusChange) {
            mGain = focusChange;
            Logger.d(TAG, "onAudioFocusChange:" , focusChange);
            if (focusChange == AudioManager.AUDIOFOCUS_LOSS ||
                    focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT) {
                if (focusChange != AudioManager.AUDIOFOCUS_LOSS) {
                    volumeREVERSE();
                    new CountDownTimer(50, 50) {
                        public void onTick(long millisUntilFinished) {
                        }

                        public void onFinish() {
                            abandomNaviTTSFocus();
                        }
                    }.start();
                }
            }
        }
    };
    private final AudioManager.OnAudioFocusChangeListener onNaviAudioFocusChangeListener = new AudioManager.OnAudioFocusChangeListener() {
        @Override
        public void onAudioFocusChange(final int focusChange) {
            mGain = focusChange;
            Logger.d(TAG, "onAudioFocusChange:" , focusChange);
            if (focusChange == AudioManager.AUDIOFOCUS_LOSS ||
                    focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT) {
                if (focusChange != AudioManager.AUDIOFOCUS_LOSS) {
                    volumeREVERSE();

                    new CountDownTimer(50, 50) {
                        public void onTick(long millisUntilFinished) {

                        }

                        public void onFinish() {
                            abandomNaviTTSFocus();
                        }
                    }.start();
                }
            }
        }
    };
    private final SparseArray<AudioTrack> mAudioTrackMap = new SparseArray<AudioTrack>();

    private NaviAudioPlayer() {
        mGain = AudioManager.AUDIOFOCUS_LOSS;
        mAudioManager = null;
        mMediaFocusRequest = null;
        mMediaAttributes = null;
        mNaviFocusRequest = null;
        mNaviPlaybackAttributes = null;
        mISSDKTTSPlaying.postValue(false);
    }

    public static NaviAudioPlayer getInstance() {
        return SingletonHolder.INSTANCE;
    }

    public void init() {
        if (null == mAudioManager) {
            mAudioManager = (AudioManager) AppCache.getInstance().getMContext().getSystemService(Context.AUDIO_SERVICE);
            //监听录音配置变化
            mAudioManager.registerAudioRecordingCallback(new AudioManager.AudioRecordingCallback() {

                public void onRecordingConfigChanged(List<AudioRecordingConfiguration> configs) {
                    super.onRecordingConfigChanged(configs);
                    for (int i = 0; i < configs.size(); i++) {
                        AudioRecordingConfiguration config = configs.get(i);
                        if (Logger.openLog) {
                            Logger.i(TAG, "onRecordingConfigChanged :", config.toString());
                        }
                        int source = config.getClientAudioSource();
                        switch (source) {
                            //录音源为麦克风
                            case MediaRecorder.AudioSource.MIC: {

                                Logger.i(TAG, "MediaRecorder.AudioSource.MIC getAudioDevice+" , config.getAudioDevice());
                            }
                            break;
                            //录音源为通话
                            case MediaRecorder.AudioSource.VOICE_COMMUNICATION:
                                Logger.d(TAG, "It is a Call");
                                Logger.d(TAG, "MediaRecorder.AudioSource.MIC getAudioDevice+" , config.getAudioDevice());
                                break;
                            default:
                                break;
                        }
                    }
                }
            }, null);
        }

        if (null == mNaviFocusRequest) {
            //AudioAttributes.Builder对象，用于构建描述音频流特性的AudioAttributes实例
            mNaviPlaybackAttributes = new AudioAttributes.Builder()
                    //设置音频流的用途为导航指导。这告诉系统，音频将用于提供导航或定位服务的语音指导，有助于系统优化音频资源分配
                    .setUsage(AudioAttributes.USAGE_ASSISTANCE_NAVIGATION_GUIDANCE)
                    //设置音频流的内容类型为语音。这有助于系统了解音频流的性质，从而做出更好的音量控制和混合决策。
                    .setContentType(AudioAttributes.CONTENT_TYPE_SPEECH)
                    .build();
            //AudioManager.AUDIOFOCUS_GAIN_TRANSIENT_MAY_DUCK表示请求短暂的音频焦点，如果其他应用正在播放音频，系统可以降低其他音频的音量（ducking）来让路给新请求
            mNaviFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN_TRANSIENT_MAY_DUCK)
                    .setAudioAttributes(mNaviPlaybackAttributes)
                    //false表示应用不接受延迟的焦点获取，即如果系统不能立即给予焦点，则应用不会开始播放音频。
                    .setAcceptsDelayedFocusGain(false)
                    .setOnAudioFocusChangeListener(onNaviAudioFocusChangeListener)
                    .build();
        }
        if (null == mMediaFocusRequest) {
            AudioAttributes.Builder builder = new AudioAttributes.Builder();
            setUsageSafety(builder);
            mMediaAttributes = builder
                    .setContentType(AudioAttributes.CONTENT_TYPE_MUSIC)//设置音频流的内容类型为音乐
                    .build();
            //请求永久的音频焦点
            mMediaFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN)
                    .setAudioAttributes(mMediaAttributes)
                    .setAcceptsDelayedFocusGain(false)
                    .setOnAudioFocusChangeListener(onMediaAudioFocusChangeListener)
                    .build();
        }
    }

    private void setUsageSafety(AudioAttributes.Builder builder) {
        int USAGE_SAFETY = -1; // 默认值（如果反射失败）
        try {
            Class<?> audioAttributesClass = Class.forName("android.media.AudioAttributes");
            Field usageSafetyField = audioAttributesClass.getDeclaredField("USAGE_SAFETY");
            usageSafetyField.setAccessible(true);
            USAGE_SAFETY = (int) usageSafetyField.get(null);
            Logger.i(TAG, "USAGE_SAFETY 1：" , USAGE_SAFETY);

            Class<? extends AudioAttributes.Builder> aClass = builder.getClass();
            Field field = aClass.getDeclaredField("mUsage");
            field.setAccessible(true);
            field.set(builder, USAGE_SAFETY);

            Logger.i(TAG, "setUsageSafety：" , USAGE_SAFETY , ",-->" , field.get(builder));
        } catch (Exception e) {
            Logger.e(TAG, "Failed to get ", e);
            builder.setUsage(AudioAttributes.USAGE_MEDIA);//设置音频流的用途为媒体播放
        }
    }

    private boolean requestNaviTTSFocus() {
        if (AudioManager.AUDIOFOCUS_REQUEST_GRANTED == mGain) {
            return true;
        }
        if (null == mAudioManager) {
            init();
        }
        mGain = mAudioManager.requestAudioFocus(mIsNormalTTS ? mNaviFocusRequest : mMediaFocusRequest);
        Logger.d(TAG, "requestNaviAudioFocus:" , mGain , ",mIsNormalTTS：" , mIsNormalTTS);
        return AudioManager.AUDIOFOCUS_REQUEST_GRANTED == mGain;
    }

    public synchronized void abandomNaviTTSFocus() {
        if (mGain == AudioManager.AUDIOFOCUS_LOSS) {
            return;
        }
        Logger.d(TAG, "abandomNaviTTSFocus");
        if (mNaviFocusRequest != null) {
            mAudioManager.abandonAudioFocusRequest(mNaviFocusRequest);
        }
        if (mNaviFocusRequest != null) {
            mAudioManager.abandonAudioFocusRequest(mMediaFocusRequest);
        }
        mGain = AudioManager.AUDIOFOCUS_LOSS;
    }

    private void volumePLAY() {
        if (null != mVolumeShaper) {
            mVolumeShaper.apply(VolumeShaper.Operation.PLAY);
        }
    }

    private void volumeREVERSE() {
        if (null != mVolumeShaper) {
            mVolumeShaper.apply(VolumeShaper.Operation.REVERSE);
        }
    }

    public AudioTrack createAudioTrack(int reqId, int sampleRate) {
        return createAudioTrack(reqId, sampleRate, true);
    }

    public AudioTrack createAudioTrack(int reqId, int sampleRate, boolean isNormalTTS) {
        Logger.d(TAG, "createAudioTrack isNormalTTS ：" , isNormalTTS , ",reqId：" , reqId);
        mIsNormalTTS = isNormalTTS;
        //计算AudioTrack的最小缓冲区大小，用于优化音频性能和避免音频抖动
        int bufsize = AudioTrack.getMinBufferSize(sampleRate,
                AudioFormat.CHANNEL_OUT_MONO,
                AudioFormat.ENCODING_PCM_16BIT);
        if (null == mNaviPlaybackAttributes || null == mMediaAttributes) {
            init();
        }
        AudioTrack audioTrack = new AudioTrack(isNormalTTS ? mNaviPlaybackAttributes : mMediaAttributes,
                new AudioFormat.Builder().setSampleRate(sampleRate)
                        .setEncoding(AudioFormat.ENCODING_PCM_16BIT)
                        .setChannelMask(AudioFormat.CHANNEL_OUT_MONO)
                        .build(),
                bufsize,
                AudioTrack.MODE_STREAM,
                AudioManager.AUDIO_SESSION_ID_GENERATE);
        //使用map为了处理多个音频输出
        mAudioTrackMap.put(reqId, audioTrack);
        if (audioTrack.getState() == 0) {
            Logger.e(TAG, "createAudioTrack State ：" + audioTrack.getState());
            return null;
        }
        mVolumeShaper = audioTrack.createVolumeShaper(configurationFadeFast);
        audioTrack.play();
        return audioTrack;
    }

    /***播放融合服务后的pcm数据***/
    public void playPcmData(int reqId, byte[] data, int sampleRate) {
        if (!requestNaviTTSFocus()) {
            return;
        }
        if (!mISSDKTTSPlaying.getValue()) {
            volumePLAY();
            mISSDKTTSPlaying.postValue(true);
        }
        AudioTrack audioTrack = mAudioTrackMap.get(reqId);
        Logger.d(TAG, "playTTS audioTrack reqId = " + reqId);
        if (null == audioTrack || audioTrack.getSampleRate() != sampleRate) {
            audioTrack = createAudioTrack(reqId, sampleRate, mIsNormalTTS);
        }
        if (null == audioTrack) {
            Logger.d(TAG, "audioTrack is null");
            return;
        }
        if (AudioTrack.STATE_UNINITIALIZED == audioTrack.getState()) {
            Logger.d(TAG, "audioTrack is not initialized");
            return;
        }
        float vol = (float) 50;
        audioTrack.setVolume(vol / 100.0f);
        audioTrack.write(data, 0, data.length);
    }

    public void releaseAudioTrack(int reqId) {
        AudioTrack audioTrack = mAudioTrackMap.get(reqId);
        if (null != audioTrack) {
            audioTrack.stop();
            audioTrack.release();
            audioTrack = null;
        }
        mAudioTrackMap.remove(reqId);
        mISSDKTTSPlaying.postValue(false);
        abandomNaviTTSFocus();
    }

    public boolean isTTSPlaying() {
        return mISSDKTTSPlaying.getValue();
    }

    private static class SingletonHolder {
        private static final NaviAudioPlayer INSTANCE = new NaviAudioPlayer();
    }
}
