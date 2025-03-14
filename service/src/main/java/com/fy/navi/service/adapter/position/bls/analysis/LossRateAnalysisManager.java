package com.fy.navi.service.adapter.position.bls.analysis;

import android.os.Handler;


import com.android.utils.thread.LooperType;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.bls.dr.DRLogService;
import com.fy.navi.service.adapter.position.bls.listener.ILossRateAnalysisInterface;
import com.fy.navi.service.adapter.position.bls.listener.ILossRateAnalysisListener;
import com.fy.navi.service.define.position.LocGpgsvAttributes;
import com.fy.navi.service.define.position.SensorCalibrationPara;


/***数据分析 逻辑暂时保留***/
public class LossRateAnalysisManager implements ILossRateAnalysisInterface {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private static final int ANALYSIS_COUNT = 1000;
    private static final float GYRO_RATE = 12.5f;
    private static final float ACC_RATE = 12.5f;
    private static final float PLUSE_RATE = 10.0f;
    private static final float GNSS_RATE = 1.0f;
    //gps 统计周期，600个gps点统计一次
    private static final int GNSS_ANALYSIS_COUNT = 600;
    //gsv检测周期，ms; 3分钟统计一次
    private static final long GSV_REPORT_INTERVAL = 3 * 60 * 1000;

    private final LossRateAnalysis mGnssLossRateAnalysis;
    private final DrStatusAnalysis mDrStatusAnalysis;
    private final GSVStatusAnalysis mGSVStatusAnalysis;
    private final ILossRateAnalysisListener mListener;
    private boolean mIsDrMode;//是否开启DR模式
    private Handler mHandler;

    public LossRateAnalysisManager(ILossRateAnalysisListener listener, DRLogService drLogService) {
        mListener = listener;
        mHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.CommonBackGroundLooper));
        mGSVStatusAnalysis = new GSVStatusAnalysis(mHandler, drLogService, this, GSV_REPORT_INTERVAL);
        mGnssLossRateAnalysis = new LossRateAnalysis(mHandler, drLogService, AnalysisType.GNSS, GNSS_RATE, GNSS_ANALYSIS_COUNT, true, this);
        mDrStatusAnalysis = new DrStatusAnalysis();
    }

    public void refreshLossRateAnalysisResult(AnalysisType type, String result) {
        if (mIsDrMode) {
            StringBuilder builder = new StringBuilder();
            builder.append(mGnssLossRateAnalysis.getAnalysisResult())
                    .append("\n");
            builder.append(mGSVStatusAnalysis.getAnalysisResult())
                    .append("\n");
            mListener.onLossRateAnalysisResult(type, builder.toString());
        }
    }

    public void setDrMode(boolean drMode) {
        mIsDrMode = drMode;
    }

    @Override
    public void analysis(AnalysisType type) {
        switch (type) {
            case GNSS:
                mGnssLossRateAnalysis.count();
                break;
            default:
                break;
        }
    }

    @Override
    public void analysisSensorPara(String dr) {
        mDrStatusAnalysis.analysis(dr);
    }

    public void analysisGSV(LocGpgsvAttributes attributes) {
        mGSVStatusAnalysis.analysisGSV(attributes);
    }
}
