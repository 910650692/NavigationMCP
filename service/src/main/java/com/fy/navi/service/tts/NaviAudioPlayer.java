package com.fy.navi.service.tts;

import android.content.Context;
import android.content.res.AssetFileDescriptor;
import android.media.AudioAttributes;
import android.media.AudioFocusRequest;
import android.media.AudioFormat;
import android.media.AudioManager;
import android.media.AudioRecordingConfiguration;
import android.media.AudioTrack;
import android.media.MediaPlayer;
import android.media.MediaRecorder;
import android.media.VolumeShaper;
import android.os.CountDownTimer;
import android.util.SparseArray;

import androidx.lifecycle.MutableLiveData;

import com.android.utils.log.Logger;
import com.autonavi.gbl.guide.model.PlayRingType;
import com.fy.navi.service.AppContext;

import java.io.IOException;
import java.util.List;

public class NaviAudioPlayer {
    private static final String TAG = "NaviAudioPlayer";
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
    private AudioAttributes mMediaPlaybackAttributes;
    private MediaPlayer mAudioPlayer;
    private AudioFocusRequest mNaviFocusRequest;
    private AudioAttributes mNaviPlaybackAttributes;
    private VolumeShaper mVolumeShaper = null;
    private final AudioManager.OnAudioFocusChangeListener onMediaAudioFocusChangeListener = new AudioManager.OnAudioFocusChangeListener() {
        @Override
        public void onAudioFocusChange(final int focusChange) {
            mGain = focusChange;
            Logger.d(TAG, "onAudioFocusChange:" + focusChange);
            if (focusChange == AudioManager.AUDIOFOCUS_LOSS ||
                    focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT) {
                if (focusChange != AudioManager.AUDIOFOCUS_LOSS) {
                    volumeREVERSE();

                    new CountDownTimer(50, 50) {
                        public void onTick(long millisUntilFinished) {

                        }
                        public void onFinish() {
                            stopAudioPlay();
                            abandomMediaAudioFocus();
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
            Logger.d(TAG, "onAudioFocusChange:" + focusChange);
            if (focusChange == AudioManager.AUDIOFOCUS_LOSS ||
                    focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT) {
                if (focusChange != AudioManager.AUDIOFOCUS_LOSS) {
                    volumeREVERSE();

                    new CountDownTimer(50, 50) {
                        public void onTick(long millisUntilFinished) {

                        }
                        public void onFinish() {
                            abandomNaviAudioFocus();
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
        mMediaPlaybackAttributes = null;
        mNaviFocusRequest = null;
        mNaviPlaybackAttributes = null;
        mISSDKTTSPlaying.postValue(false);
    }

    public static NaviAudioPlayer getInstance() {
        return SingletonHolder.INSTANCE;
    }

    public void init() {
        if (null == mAudioManager) {
            mAudioManager = (AudioManager) AppContext.getInstance().getMContext().getSystemService(Context.AUDIO_SERVICE);
            //监听录音配置变化
            mAudioManager.registerAudioRecordingCallback(new AudioManager.AudioRecordingCallback() {

                public void onRecordingConfigChanged(List<AudioRecordingConfiguration> configs) {
                    super.onRecordingConfigChanged(configs);
                    for (int i = 0; i < configs.size(); i++) {
                        AudioRecordingConfiguration config = configs.get(i);
                        Logger.i(TAG, "onRecordingConfigChanged :" + config.toString());
                        int source = config.getClientAudioSource();
                        switch (source) {
                            //录音源为麦克风
                            case MediaRecorder.AudioSource.MIC: {

                                Logger.i(TAG, "MediaRecorder.AudioSource.MIC getAudioDevice+" + config.getAudioDevice());
                            }
                            break;
                            //录音源为通话
                            case MediaRecorder.AudioSource.VOICE_COMMUNICATION:
                                Logger.d(TAG, "It is a Call");
                                Logger.d(TAG, "MediaRecorder.AudioSource.MIC getAudioDevice+" + config.getAudioDevice());
                                break;
                            default:
                                break;
                        }
                    }
                }
            }, null);
        }
        if (null == mMediaFocusRequest) {
            mMediaPlaybackAttributes = new AudioAttributes.Builder()
                    //设置音频流的用途为媒体播放
                    .setUsage(AudioAttributes.USAGE_MEDIA)
                    //设置音频流的内容类型为音乐
                    .setContentType(AudioAttributes.CONTENT_TYPE_MUSIC)
                    .build();
            //请求永久的音频焦点
            mMediaFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN)
                    .setAudioAttributes(mMediaPlaybackAttributes)
                    .setAcceptsDelayedFocusGain(false)
                    .setOnAudioFocusChangeListener(onMediaAudioFocusChangeListener)
                    .build();
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
    }

    private boolean requestMediaAudioFocus() {
        if (null == mAudioManager) {
            init();
        }
        Logger.d(TAG, "requestMediaAudioFocus");

        mGain = mAudioManager.requestAudioFocus(mMediaFocusRequest);
        Logger.d(TAG, "onAudioFocusChange:" + mGain);
        return AudioManager.AUDIOFOCUS_REQUEST_GRANTED == mGain;
    }

    private synchronized void abandomMediaAudioFocus() {
        if (mGain == AudioManager.AUDIOFOCUS_LOSS) {
            return;
        }
        if (mMediaFocusRequest != null) {
            Logger.d(TAG, "abandomMediaAudioFocus");
            mAudioManager.abandonAudioFocusRequest(mMediaFocusRequest);
        }
        mGain = AudioManager.AUDIOFOCUS_LOSS;
    }

    private boolean requestNaviAudioFocus() {
        if (AudioManager.AUDIOFOCUS_REQUEST_GRANTED == mGain) {
            return true;
        }

        if (null == mAudioManager) {
            init();
        }
        Logger.d(TAG, "requestNaviAudioFocus");

        mGain = mAudioManager.requestAudioFocus(mNaviFocusRequest);
        Logger.d(TAG, "onAudioFocusChange:" + mGain);
        return AudioManager.AUDIOFOCUS_REQUEST_GRANTED == mGain;
    }

    public synchronized void abandomNaviAudioFocus() {
        if (mGain == AudioManager.AUDIOFOCUS_LOSS) {
            return;
        }
        if (mNaviFocusRequest != null) {
            Logger.d(TAG, "abandomNaviAudioFocus");
            mAudioManager.abandonAudioFocusRequest(mNaviFocusRequest);
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

    public void stopAudioPlay() {
        if (mAudioPlayer != null) {
            mAudioPlayer.stop();
            mAudioPlayer.release();
            mAudioPlayer = null;
            Logger.d(TAG, "stopAudioPlay");
        }
    }

    public void stopGblTtsPlayer() {
//        SpeechController.getInstance().stop();
    }

    public List<AudioRecordingConfiguration> getActiveRecordingConfigurations() {
        return mAudioManager.getActiveRecordingConfigurations();
    }

    /***此处调用高德合成服务接口***/
    public void playTTS(String text) {
        Logger.d(TAG, "playTTS: " + text);
//        SpeechController.getInstance().synthesize(text);
    }

    /***播放路口提示音***/
    public void playNaviWarningSound(int type) {
        int resId = 0;
        switch (type) {
            case PlayRingType.PlayRingTypeDing:
//                resId = R.raw.navi_warning;
                break;
            case PlayRingType.PlayRingTypeDong:
//                resId = R.raw.camera;
                break;
            case PlayRingType.PlayRingTypeElecDing:
//                resId = R.raw.edog_dingdong;
                break;
            case PlayRingType.PlayRingTypeReroute:
//                resId = R.raw.autoreroute;
                break;
            default:
                break;
        }
        if (!requestNaviAudioFocus()) {
            return;
        }
        mAudioPlayer = new MediaPlayer();
        AssetFileDescriptor file = null;
        try {
            file = AppContext.getInstance().getMContext().getApplicationContext().getResources().openRawResourceFd(resId);
            mAudioPlayer.setAudioAttributes(mNaviPlaybackAttributes);
            mAudioPlayer.setDataSource(file.getFileDescriptor(),
                    file.getStartOffset(), file.getLength());
            mAudioPlayer.prepareAsync();
            mAudioPlayer.setOnPreparedListener(new MediaPlayer.OnPreparedListener() {
                @Override
                public void onPrepared(MediaPlayer mp) {
                    Logger.d(TAG, "playNaviWarningSound(). start play");
                    mp.start();
                }
            });
            mAudioPlayer.setOnCompletionListener(new MediaPlayer.OnCompletionListener() {
                @Override
                public void onCompletion(MediaPlayer mp) {
                    Logger.d(TAG, "playNaviWarningSound(). oncomplete");
                    stopAudioPlay();
                    if (!isTTSPlaying()) {
                        abandomNaviAudioFocus();
                    }
                }
            });
            mAudioPlayer.setOnErrorListener(new MediaPlayer.OnErrorListener() {
                @Override
                public boolean onError(MediaPlayer mp, int what, int extra) {
                    Logger.d(TAG, "playNaviWarningSound(). onerror");
                    stopAudioPlay();
                    if (!isTTSPlaying()) {
                        abandomNaviAudioFocus();
                    }
                    return false;
                }
            });
        } catch (Exception e) {
            Logger.e(TAG, "playNaviWarningSound(). Exception " + e);
        } finally {
            Logger.d(TAG, "playNaviWarningSound(). finally");
            try {
                if (file != null) {
                    file.close();
                }

            } catch (IOException e) {
                Logger.e(TAG, "playNaviWarningSound().  MediaPlayer IOException msg=" + e.getMessage());
            }
        }
    }

    public AudioTrack createAudioTrack(int reqId, int sampleRate) {
        //计算AudioTrack的最小缓冲区大小，用于优化音频性能和避免音频抖动
        int bufsize = AudioTrack.getMinBufferSize(sampleRate,
                AudioFormat.CHANNEL_OUT_MONO,
                AudioFormat.ENCODING_PCM_16BIT);

        if (null == mNaviPlaybackAttributes) {
            init();
        }
        AudioTrack audioTrack = new AudioTrack(mNaviPlaybackAttributes,
                new AudioFormat.Builder().setSampleRate(sampleRate)
                        .setEncoding(AudioFormat.ENCODING_PCM_16BIT)
                        .setChannelMask(AudioFormat.CHANNEL_OUT_MONO)
                        .build(),
                bufsize,
                AudioTrack.MODE_STREAM,
                AudioManager.AUDIO_SESSION_ID_GENERATE);
        //使用map为了处理多个音频输出
        mAudioTrackMap.put(reqId, audioTrack);
        mVolumeShaper = audioTrack.createVolumeShaper(configurationFadeFast);
        audioTrack.play();
        return audioTrack;
    }

    /***播放融合服务后的pcm数据***/
    public void playPcmData(int reqId, byte[] data, int sampleRate) {
        if (!requestNaviAudioFocus()) {
            return;
        }
        if (!mISSDKTTSPlaying.getValue()) {
            volumePLAY();
            mISSDKTTSPlaying.postValue(true);
        }
        AudioTrack audioTrack = mAudioTrackMap.get(reqId);
        Logger.d(TAG, "playTTS audioTrack getStreamType = " + audioTrack.getStreamType());
        if (null == audioTrack || audioTrack.getSampleRate() != sampleRate) {
            audioTrack = createAudioTrack(reqId, sampleRate);
        }

        if (null == audioTrack) {
            Logger.d(TAG, "audioTrack is null");
            return;
        }
        if (AudioTrack.STATE_UNINITIALIZED == audioTrack.getState()) {
            Logger.d(TAG, "audioTrack is not initialized");
            return;
        }
        //TODO: 添加音量
//        SdkSharePreference sdkSharePreference = new SdkSharePreference(, SdkSharePreference.SharePreferenceName.prefer)
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
        abandomNaviAudioFocus();
    }

    public boolean isTTSPlaying() {
        return mISSDKTTSPlaying.getValue();
    }

    private static class SingletonHolder {
        private static final NaviAudioPlayer INSTANCE = new NaviAudioPlayer();
    }
}
