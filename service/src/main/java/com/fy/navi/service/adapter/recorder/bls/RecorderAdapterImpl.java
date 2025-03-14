package com.fy.navi.service.adapter.recorder.bls;

import com.android.utils.gson.GsonUtils;
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

    private RecorderService recorderSrv;
    private Recorder recorder;
    private Player player;
    private PlayProgressInfo playProgressInfo;
    private final List<RecorderAdapterCallback> callBacks = new ArrayList<>();

    @Override
    public void initService() {
        Logger.d("Recording init start.");
        // 获取录制回放服务（一级服务）
        recorderSrv = (RecorderService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.RecorderSingleServiceID);

        // 获取录制服务
        recorder = recorderSrv.getRecorder();
        player = recorderSrv.getPlayer();

        playProgressInfo = new PlayProgressInfo();
        Logger.d("Recording init success.");
    }

    /**
     * 开始录制
     */
    @Override
    public void startRecorder() {
        if (recorder != null) {
            recorder.start();
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
        if (recorder != null) {
            recorder.stop();
            Logger.d("Recording stopped.");
        } else {
            Logger.d("Recorder is not initialized.");
        }    }

    /**
     * 开始回放
     */
    @Override
    public void startPlayback() {
        if (player != null) {
            player.start();
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
        if (player != null) {
            player.stop();
            player.removeObserver(this);
            Logger.d("Playback stopped.");
        } else {
            Logger.d("Recorder service is not initialized.");
        }
    }

    @Override
    public void registerCallBack(String key, RecorderAdapterCallback callBack) {
        callBacks.add(callBack);
    }

    /**
     * 回放进度通知
     * @param playProgress 进知通知信息
     */
    @Override
    public void onPlayProgress(PlayProgress playProgress) {

        GsonUtils.copyBean(playProgress, playProgressInfo);

        Logger.d("Playback progress: " + playProgress.currentMessageIndex + "/" + playProgress.totalMessageCount);

        for (RecorderAdapterCallback observer : callBacks) {
            observer.notifyPlayProgress(playProgressInfo);
        }
    }
}
