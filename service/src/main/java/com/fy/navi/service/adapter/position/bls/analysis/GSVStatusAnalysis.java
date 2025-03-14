package com.fy.navi.service.adapter.position.bls.analysis;

import android.os.Handler;
import android.os.SystemClock;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.bls.dr.DRLogService;
import com.fy.navi.service.define.position.LocGpgsvAttributes;

/***星历分析***/
public class GSVStatusAnalysis extends BaseFusionStatusAnalysis {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    //一个周期内 信噪比总和
    private int mTotalSnr = 0;
    //一个周期内 信噪比个数总和
    private int mTotalCount = 0;
    private int mGoodSnrCount = 0;
    private final Object mLocked = new Object();
    private long mLastTimeStamp = -1;
    private final long mGsvReportInterval;

    public GSVStatusAnalysis(Handler handler, DRLogService drLogService, LossRateAnalysisManager analysisManager, long reportInterval) {
        super(handler, drLogService, analysisManager, 1000);
        mGsvReportInterval = reportInterval;
    }

    private final Runnable mRunnable = new Runnable() {
        @Override
        public void run() {
            mHandler.removeCallbacks(mRunnable);
            mHandler.postDelayed(mRunnable, mGsvReportInterval);

            int totalSnr;
            int totalCount;
            int goodCount;
            synchronized (mLocked) {
                totalSnr = mTotalSnr;
                totalCount = mTotalCount;
                goodCount = mGoodSnrCount;
                mTotalCount = 0;
                mTotalSnr = 0;
                mGoodSnrCount = 0;
            }
            int averageSnr = -1;
            float goodRate = -1;
            if (totalCount > 0) {
                averageSnr = totalSnr / totalCount;
                goodRate = (goodCount * 10000.0f) / (100 * totalCount);
            }
            String lossRate = getLossRate(mLastTimeStamp, totalCount);
            StringBuilder builder = new StringBuilder();
            builder.append("GSV:")
                    .append(averageSnr)
                    .append(";" + goodRate + "%")
                    .append(";" + lossRate);
            mAnalysisResult = builder.toString();
            mAnalysisManager.refreshLossRateAnalysisResult(AnalysisType.GYR, mAnalysisResult);
            builder.append(";" + System.currentTimeMillis());
            String s = builder.toString();
            Logger.i(TAG, s);
            mLastTimeStamp = SystemClock.elapsedRealtime();
        }
    };

    public void analysisGSV(LocGpgsvAttributes attributes) {
        if (mLastTimeStamp == -1) {
            mLastTimeStamp = SystemClock.elapsedRealtime();
            mHandler.postDelayed(mRunnable, mGsvReportInterval);
            Logger.i(TAG, "start analysisGSV");
        }
        synchronized (mLocked) {
            mTotalCount++;
            mTotalSnr += attributes.averageSnr;
            if (attributes.averageSnr > 30) {
                mGoodSnrCount++;
            }
        }
    }
}
