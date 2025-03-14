package com.fy.navi.service.adapter.position.bls.listener;

import com.fy.navi.service.adapter.position.bls.analysis.AnalysisType;

public interface ILossRateAnalysisInterface {
    void analysis(AnalysisType type);

    void analysisSensorPara(String dr);
}
