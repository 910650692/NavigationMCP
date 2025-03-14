package com.fy.navi.service.logicpaket.recorder;

import com.fy.navi.service.adapter.recorder.RecorderAdapter;
import com.fy.navi.service.adapter.recorder.RecorderAdapterCallback;
import com.fy.navi.service.define.recorder.PlayProgressInfo;

import java.util.ArrayList;
import java.util.List;


public class RecorderPackage implements RecorderAdapterCallback {

    private RecorderAdapter mRecorderAdapter;
    private final List<RecorderCallBack> callBacks = new ArrayList<>();


    public RecorderPackage() {
        mRecorderAdapter = RecorderAdapter.getInstance();
    }

    public static RecorderPackage getInstance() {
        return RecorderPackage.SInstanceHolder.sInstance;
    }

    @Override
    public void initService() {
        mRecorderAdapter.initService();
        mRecorderAdapter.registerCallBack("RecorderPackage", this);
    }

    public void startRecorder() {
        mRecorderAdapter.startRecorder();
    }

    public void stopRecorder() {
        mRecorderAdapter.stopRecorder();
    }

    public void startPlayback() {
        mRecorderAdapter.startPlayback();
    }

    public void stopPlayback() {
        mRecorderAdapter.stopPlayback();
    }

    public synchronized void registerCallBack(RecorderCallBack callback) {
        if (callback != null && !callBacks.contains(callback)) {
            callBacks.add(callback);
        }
    }

    @Override
    public void notifyPlayProgress(PlayProgressInfo playProgressInfo) {
        if (null != callBacks) {
            for (RecorderCallBack observer : callBacks) {
                observer.notifyPlayProgress(playProgressInfo);
            }
        }
    }

    private static final class SInstanceHolder {
        static final RecorderPackage sInstance = new RecorderPackage();
    }

}
