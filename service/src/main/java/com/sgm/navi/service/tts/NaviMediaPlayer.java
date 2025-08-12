package com.sgm.navi.service.tts;


import static com.sgm.navi.service.MapDefaultFinalTag.NAVI_SERVICE_MEDIA;

import android.content.Context;
import android.media.AudioAttributes;
import android.media.AudioFocusRequest;
import android.media.AudioManager;
import android.media.MediaPlayer;

import com.android.utils.log.Logger;
import com.autonavi.gbl.guide.model.PlayRingType;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.R;

import lombok.Setter;

public class NaviMediaPlayer {
    private static final String TAG = NAVI_SERVICE_MEDIA;
    @Setter
    private AudioManager mAudioManager;
    private AudioFocusRequest mMediaFocusRequest;
    private MediaPlayer mMediaPlayer;

    public static NaviMediaPlayer getInstance() {
        return NaviMediaPlayer.SingletonHolder.INSTANCE;
    }

    private static class SingletonHolder {
        private static final NaviMediaPlayer INSTANCE = new NaviMediaPlayer();
    }

    public void init() {
        if (null == mAudioManager) {
            NaviAudioPlayer.getInstance().init();
        }
        if (null == mMediaFocusRequest) {
            AudioAttributes.Builder builder = new AudioAttributes.Builder();
            AudioAttributes mediaAttributes = builder
                    .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
                    .build();
            //请求临时音频焦点
            mMediaFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN_TRANSIENT)
                    .setAudioAttributes(mediaAttributes)
                    .setAcceptsDelayedFocusGain(false)
                    .setOnAudioFocusChangeListener(onMediaAudioFocusChangeListener)
                    .build();
        }
    }

    private final AudioManager.OnAudioFocusChangeListener onMediaAudioFocusChangeListener = new AudioManager.OnAudioFocusChangeListener() {
        @Override
        public void onAudioFocusChange(final int focusChange) {
            Logger.d(TAG, "focusChange:", focusChange);
            switch (focusChange) {
                case AudioManager.AUDIOFOCUS_GAIN:
                    // 成功获取焦点（或重新获取），可以开始/恢复播放
                    break;
                case AudioManager.AUDIOFOCUS_LOSS:
                    // 永久丢失焦点（如其他应用长期占用），停止播放并释放资源
                    break;
                case AudioManager.AUDIOFOCUS_LOSS_TRANSIENT:
                    // 临时丢失焦点（如来电、导航播报），暂停播放，等待焦点恢复
                    break;
                case AudioManager.AUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK:
                    // 临时丢失焦点但可降低音量（如视频通话时背景音降低）
                    break;
            }
        }
    };

    private boolean getAudioFocus() {
        if (mAudioManager == null || mMediaFocusRequest == null) {
            init();
            return false;
        }
        int result = mAudioManager.requestAudioFocus(mMediaFocusRequest);
        Logger.d(TAG, "getAudioFocus result:", result);
        return result == AudioManager.AUDIOFOCUS_REQUEST_GRANTED;
    }


    /**
     * 导航叮叮音播报
     *
     * @param type
     */
    public void playNaviWarningSound(int type) {
        int resId = 0;
        switch (type) {
            /*case PlayRingType.PlayRingTypeDing:         // 导航通过音
                resId = R.raw.navi_warning;
                break;*/
            case PlayRingType.PlayRingTypeDong:         // 电子眼高速通过音
                resId = R.raw.camera;
                break;
            case PlayRingType.PlayRingTypeElecDing:     // 电子狗的叮咚声
                resId = R.raw.edog_dingdong;
                break;
            case PlayRingType.PlayRingTypeReroute:      // 偏航提示音
                resId = R.raw.autoreroute;
                break;
            default:
                break;
        }
        if (resId == 0) {
            Logger.i(TAG, "type: ", type);
            return;
        }
        try {
            if (!getAudioFocus()) {
                return;
            }
            if (mMediaPlayer != null) {
                if (mMediaPlayer.isPlaying()) {
                    mMediaPlayer.stop();
                }
                mMediaPlayer.release();
                mMediaPlayer = null;
            }
            mMediaPlayer = MediaPlayer.create(AppCache.getInstance().getMContext(), resId);
            mMediaPlayer.setOnCompletionListener(new MediaPlayer.OnCompletionListener() {
                @Override
                public void onCompletion(MediaPlayer mp) {
                    Logger.d(TAG, "onCompletion");
                    if (mAudioManager != null && mMediaFocusRequest != null) {
                        mAudioManager.abandonAudioFocusRequest(mMediaFocusRequest);
                    }
                }
            });
            mMediaPlayer.setOnErrorListener(new MediaPlayer.OnErrorListener() {
                @Override
                public boolean onError(MediaPlayer mp, int what, int extra) {
                    Logger.e(TAG, "onError what:", what, " extra:", extra);
                    if (mAudioManager != null && mMediaFocusRequest != null) {
                        mAudioManager.abandonAudioFocusRequest(mMediaFocusRequest);
                    }
                    return false;
                }
            });
            //TODO 叮咚声音量跟随导航 暂无接口
            float volume = 1.0f;
            mMediaPlayer.setVolume(volume, volume);
            mMediaPlayer.start();
        } catch (Exception e) {
            Logger.e(TAG, "e:", e.getMessage());
            e.printStackTrace();
        }
    }

    public void releaseMediaPlayer() {
        if (mMediaPlayer != null) {
            if (mMediaPlayer.isPlaying()) {
                mMediaPlayer.stop();
            }
            mMediaPlayer = null;
        }
    }

}
