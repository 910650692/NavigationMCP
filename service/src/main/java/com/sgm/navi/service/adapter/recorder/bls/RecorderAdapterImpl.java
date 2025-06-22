package com.sgm.navi.service.adapter.recorder.bls;

import com.android.utils.log.Logger;
import com.autonavi.gbl.recorder.Player;
import com.autonavi.gbl.recorder.Recorder;
import com.autonavi.gbl.recorder.RecorderService;
import com.autonavi.gbl.recorder.model.PlayParam;
import com.autonavi.gbl.recorder.model.PlayProgress;
import com.autonavi.gbl.recorder.observer.IPlayerObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.adapter.recorder.IRecorderApi;
import com.sgm.navi.service.adapter.recorder.RecorderAdapterCallback;
import com.sgm.navi.service.define.recorder.PlayProgressInfo;

import java.util.ArrayList;
import java.util.List;

public class RecorderAdapterImpl implements IRecorderApi, IPlayerObserver {
    private static final String TAG = RecorderAdapterImpl.class.getSimpleName();
    private RecorderService mRecorderSrv;
    private Recorder mRecorder;
    private Player mPlayer;
    private PlayProgressInfo mPlayProgressInfo;
    private final List<RecorderAdapterCallback> mCallBacks = new ArrayList<>();
    private boolean mIsRecording;
    private boolean mIsPlaying;

    @Override
    public void initService() {
        Logger.d("Recording init start.");
        // 获取录制回放服务（一级服务）
        mRecorderSrv = (RecorderService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.RecorderSingleServiceID);

        // 获取录制服务
        mRecorder = mRecorderSrv.getRecorder();
        mPlayer = mRecorderSrv.getPlayer();

        mPlayProgressInfo = new PlayProgressInfo();
        initPlayRecord();
        Logger.d(TAG,"Recording init success.");
    }

    /**
     * 开始录制
     */
    @Override
    public void startRecorder() {
        if (mRecorder != null) {
            mIsRecording = true;
            mRecorder.start();
            Logger.d(TAG,"Recording started.");
        } else {
            Logger.d(TAG,"Recorder is not initialized.");
        }
    }

    /**
     * 停止录制
     */
    @Override
    public void stopRecorder() {
        if (mRecorder != null) {
            mIsRecording = false;
            mRecorder.stop();
            Logger.d(TAG,"Recording stopped.");
        } else {
            Logger.d(TAG,"Recorder is not initialized.");
        }
    }

    @Override
    public boolean isRecording() {
        return mIsRecording;
    }

    /**
     * 初始化回放
     */
    public void initPlayRecord() {
        if (mPlayer != null) {
            PlayParam param = new PlayParam();
            param.isLooping = true;
            param.playPath = GBLCacheFilePath.MAP_RECORD_PATH;
            mPlayer.setParam(param);
            Logger.d(TAG,"Player set PlayParam.");
        } else {
            Logger.d(TAG,"Player is not initialized.");
        }
    }

    /**
     * 开始回放
     */
    @Override
    public void startPlayback() {
        if (mPlayer != null) {
            mIsPlaying = true;
            mPlayer.start();
            Logger.d(TAG,"Playback started.");
        } else {
            Logger.d(TAG,"Recorder service is not initialized.");
        }
    }

    /**
     * 停止回放
     */
    @Override
    public void stopPlayback() {
        if (mPlayer != null) {
            mIsPlaying = false;
            mPlayer.stop();
            mPlayer.removeObserver(this);
            Logger.d(TAG,"Playback stopped.");
        } else {
            Logger.d(TAG,"Recorder service is not initialized.");
        }
    }

    @Override
    public boolean isPlaying() {
        return mIsPlaying;
    }

    @Override
    public void registerCallBack(final String key, final RecorderAdapterCallback callBack) {
        mCallBacks.add(callBack);
    }

    /**
     * 回放进度通知
     *
     * @param playProgress 进知通知信息
     */
    @Override
    public void onPlayProgress(final PlayProgress playProgress) {

        setPlayProgressInfo(playProgress);

        Logger.d(TAG,"Playback progress: " + playProgress.currentMessageIndex + "/" + playProgress.totalMessageCount);

        for (RecorderAdapterCallback observer : mCallBacks) {
            observer.notifyPlayProgress(mPlayProgressInfo);
        }
    }

    /**
     * 设置回放进度信息
     *
     * @param playProgress 回放进度信息
     */
    private void setPlayProgressInfo(final PlayProgress playProgress) {
        mPlayProgressInfo.setFileIndex(playProgress.fileIndex);
        mPlayProgressInfo.setFileTotalCount(playProgress.fileTotalCount);
        mPlayProgressInfo.setPlayName(playProgress.playName);
        mPlayProgressInfo.setCurrentMessageIndex(playProgress.currentMessageIndex);
        mPlayProgressInfo.setTotalMessageCount(playProgress.totalMessageCount);
        mPlayProgressInfo.setUnixTimestamp(playProgress.unixTimestamp);
    }
}
