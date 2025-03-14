package com.fy.navi.service.adapter.position.bls.listener;

import com.fy.navi.service.adapter.position.bls.analysis.AnalysisType;

public interface ILossRateAnalysisListener {
    /*gyr,acc,pluse,gnss,gyr 丢帧率 稳定性分析结果*/
    void onLossRateAnalysisResult(AnalysisType type, String allResult);
}
