package com.fy.navi.service.logicpaket.recorder;

import com.fy.navi.service.adapter.recorder.RecorderAdapter;
import com.fy.navi.service.adapter.recorder.RecorderAdapterCallback;
import com.fy.navi.service.define.recorder.PlayProgressInfo;

import java.util.ArrayList;
import java.util.List;


public class RecorderPackage implements RecorderAdapterCallback {

    private final RecorderAdapter mRecorderAdapter;
    private final List<RecorderCallBack> mCallBacks = new ArrayList<>();


    public RecorderPackage() {
        mRecorderAdapter = RecorderAdapter.getInstance();
    }

    public static RecorderPackage getInstance() {
        return RecorderPackage.SInstanceHolder.INSTANCE;
    }

    @Override
    public void initService() {
        mRecorderAdapter.initService();
        mRecorderAdapter.registerCallBack("RecorderPackage", this);
    }

    /**
     * 开始记录
     */
    public void startRecorder() {
        mRecorderAdapter.startRecorder();
    }

    /**
     * 停止记录
     */
    public void stopRecorder() {
        mRecorderAdapter.stopRecorder();
    }

    /**
     * 开始回放
     */
    public void startPlayback() {
        mRecorderAdapter.startPlayback();
    }

    /**
     * 停止回放
     */
    public void stopPlayback() {
        mRecorderAdapter.stopPlayback();
    }

    /**
     * 注册回调
     * @param callback 回调
     */
    public synchronized void registerCallBack(final RecorderCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.add(callback);
        }
    }

    @Override
    public void notifyPlayProgress(final PlayProgressInfo playProgressInfo) {
        for (RecorderCallBack observer : mCallBacks) {
            observer.notifyPlayProgress(playProgressInfo);
        }
    }

    private static final class SInstanceHolder {
        static final RecorderPackage INSTANCE = new RecorderPackage();
    }

}
