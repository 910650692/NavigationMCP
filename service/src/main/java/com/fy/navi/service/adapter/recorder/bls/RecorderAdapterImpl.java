package com.fy.navi.service.adapter.recorder.bls;

import com.android.utils.log.Logger;
import com.autonavi.gbl.recorder.Player;
import com.autonavi.gbl.recorder.Recorder;
import com.autonavi.gbl.recorder.RecorderService;
import com.autonavi.gbl.recorder.model.PlayProgress;
import com.autonavi.gbl.recorder.observer.IPlayerObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.adapter.recorder.IRecorderApi;
import com.fy.navi.service.adapter.recorder.RecorderAdapterCallback;
import com.fy.navi.service.define.recorder.PlayProgressInfo;

import java.util.ArrayList;
import java.util.List;

public class RecorderAdapterImpl implements IRecorderApi ,IPlayerObserver{

    private RecorderService mRecorderSrv;
    private Recorder mRecorder;
    private Player mPlayer;
    private PlayProgressInfo mPlayProgressInfo;
    private final List<RecorderAdapterCallback> mCallBacks = new ArrayList<>();

    @Override
    public void initService() {
        Logger.d("Recording init start.");
        // 获取录制回放服务（一级服务）
        mRecorderSrv = (RecorderService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.RecorderSingleServiceID);

        // 获取录制服务
        mRecorder = mRecorderSrv.getRecorder();
        mPlayer = mRecorderSrv.getPlayer();

        mPlayProgressInfo = new PlayProgressInfo();
        Logger.d("Recording init success.");
    }

    /**
     * 开始录制
     */
    @Override
    public void startRecorder() {
        if (mRecorder != null) {
            mRecorder.start();
            Logger.d("Recording started.");
        } else {
            Logger.d("Recorder is not initialized.");
        }
    }

    /**
     * 停止录制
     */
    @Override
    public void stopRecorder() {
        if (mRecorder != null) {
            mRecorder.stop();
            Logger.d("Recording stopped.");
        } else {
            Logger.d("Recorder is not initialized.");
        }
    }

    /**
     * 开始回放
     */
    @Override
    public void startPlayback() {
        if (mPlayer != null) {
            mPlayer.start();
            Logger.d("Playback started.");
        } else {
            Logger.d("Recorder service is not initialized.");
        }
    }

    /**
     * 停止回放
     */
    @Override
    public void stopPlayback() {
        if (mPlayer != null) {
            mPlayer.stop();
            mPlayer.removeObserver(this);
            Logger.d("Playback stopped.");
        } else {
            Logger.d("Recorder service is not initialized.");
        }
    }

    @Override
    public void registerCallBack(final String key, final RecorderAdapterCallback callBack) {
        mCallBacks.add(callBack);
    }

    /**
     * 回放进度通知
     * @param playProgress 进知通知信息
     */
    @Override
    public void onPlayProgress(final PlayProgress playProgress) {

        setPlayProgressInfo(playProgress);

        Logger.d("Playback progress: " + playProgress.currentMessageIndex + "/" + playProgress.totalMessageCount);

        for (RecorderAdapterCallback observer : mCallBacks) {
            observer.notifyPlayProgress(mPlayProgressInfo);
        }
    }

    /**
     * 设置回放进度信息
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
