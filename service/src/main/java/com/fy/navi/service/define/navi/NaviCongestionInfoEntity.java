package com.fy.navi.service.define.navi;

import java.util.ArrayList;

public class NaviCongestionInfoEntity {
    private long totalTimeOfSeconds;
    private long totalRemainDist;
    private boolean unobstructed;
    private ArrayList<NaviCongestionDetailInfoEntity> congestionInfos;

    public long getTotalTimeOfSeconds() {
        return totalTimeOfSeconds;
    }

    public void setTotalTimeOfSeconds(long totalTimeOfSeconds) {
        this.totalTimeOfSeconds = totalTimeOfSeconds;
    }

    public long getTotalRemainDist() {
        return totalRemainDist;
    }

    public void setTotalRemainDist(long totalRemainDist) {
        this.totalRemainDist = totalRemainDist;
    }

    public boolean isUnobstructed() {
        return unobstructed;
    }

    public void setUnobstructed(boolean unobstructed) {
        this.unobstructed = unobstructed;
    }

    public ArrayList<NaviCongestionDetailInfoEntity> getCongestionInfos() {
        return congestionInfos;
    }

    public void setCongestionInfos(ArrayList<NaviCongestionDetailInfoEntity> congestionInfos) {
        this.congestionInfos = congestionInfos;
    }

    @Override
    public String toString() {
        return "NaviCongestionInfoEntity{" +
                "totalTimeOfSeconds=" + totalTimeOfSeconds +
                ", totalRemainDist=" + totalRemainDist +
                ", unobstructed=" + unobstructed +
                ", congestionInfos=" + congestionInfos +
                '}';
    }
}
