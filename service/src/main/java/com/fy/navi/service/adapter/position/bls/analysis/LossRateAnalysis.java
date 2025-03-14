package com.fy.navi.service.adapter.position.bls.analysis;

import android.os.Handler;
import android.os.SystemClock;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.bls.dr.DRLogService;

import java.util.Locale;

/***Gnss分析***/
public class LossRateAnalysis extends BaseFusionStatusAnalysis {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private static final float RATIO = 0.3f;
    private int mNum = 0;
    private long mStartTime = 0;
    private final int mCount;
    private final AnalysisType mType;
    private int mUnStableCount = 0;
    private long mTimeStamp = 0;
    private final boolean mIsNeedAnalysisStable;

    public LossRateAnalysis(Handler handler, DRLogService drLogService, AnalysisType type, float rate, int count, boolean stable, LossRateAnalysisManager manager) {
        super(handler, drLogService, manager, 1000 / rate);
        mCount = count;
        mType = type;
        mIsNeedAnalysisStable = stable;
        mAnalysisResult = mType.toString().toLowerCase(Locale.ROOT) + " waiting...";
    }

    public void count() {
        if (mNum == 0) {
            mStartTime = SystemClock.elapsedRealtime();
        }
        if (mIsNeedAnalysisStable) {
            if (mTimeStamp != 0 && Math.abs(SystemClock.elapsedRealtime() - mTimeStamp - mInterval) >= RATIO * mInterval) {
                mUnStableCount++;
            }
            mTimeStamp = SystemClock.elapsedRealtime();
        }
        mNum++;
        if (mNum == mCount) {
            mHandler.post(() -> handle(mStartTime, mUnStableCount));
            mNum = 0;
            mUnStableCount = 0;
            mTimeStamp = 0;
        }
    }

    private void handle(long startTime, int unStableCount) {
        long total = SystemClock.elapsedRealtime() - startTime;
        float f = 1.0f - mCount / (total * 1.0f) * mInterval;

        StringBuilder builder = new StringBuilder();
        builder.append(mType.toString().toLowerCase(Locale.ROOT))
                .append(": ")
                .append(getLossRate(startTime, mCount))
                .append(" | ");
        if (mIsNeedAnalysisStable) {
            float uf = unStableCount / (mCount * 1.0f);
            builder.append(((int) (uf * 10000) / 100.0f) + "%")
                    .append(" | ");
        }
        builder.append(mInterval)
                .append(" | ")
                .append((int) ((total * 1.0f) / mCount * 100) / 100);
        mAnalysisResult = builder.toString();
        builder.append("  total used time:" + total);
        if (mIsNeedAnalysisStable) {
            builder.append(" unstable count:" + unStableCount);
            builder.append(" total count:" + mCount);
        }
        mAnalysisResult = builder.toString();
        mAnalysisManager.refreshLossRateAnalysisResult(mType, mAnalysisResult);
        builder.append(" | ");
        builder.append(System.currentTimeMillis());
        String s = builder.toString();
        Logger.i(TAG, s);
    }

    public void reset() {
        mNum = 0;
    }
}
