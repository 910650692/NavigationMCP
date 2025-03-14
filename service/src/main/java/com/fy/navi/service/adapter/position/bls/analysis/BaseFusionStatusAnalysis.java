package com.fy.navi.service.adapter.position.bls.analysis;

import android.os.Handler;
import android.os.SystemClock;

import com.fy.navi.service.adapter.position.bls.dr.DRLogService;

public class BaseFusionStatusAnalysis {
    protected String mAnalysisResult;
    protected final Handler mHandler;
    protected final DRLogService mDrLogService;
    protected final float mInterval; //信号间隔，单位ms
    protected final LossRateAnalysisManager mAnalysisManager;

    public BaseFusionStatusAnalysis(Handler handler, DRLogService drLogService, LossRateAnalysisManager analysisManager, float interval) {
        mHandler = handler;
        mDrLogService = drLogService;
        mAnalysisManager = analysisManager;
        mInterval = interval;
    }

    public String getAnalysisResult() {
        return mAnalysisResult;
    }

    protected String getLossRate(long StartMills, int totalCount) {
        long total = SystemClock.elapsedRealtime() - StartMills;
        float f = 1.0f - totalCount / (total * 1.0f) * mInterval;
        return ((int) (f * 10000) / 100.0f) + "%";
    }
}
