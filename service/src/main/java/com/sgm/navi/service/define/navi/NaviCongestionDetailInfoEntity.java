package com.sgm.navi.service.define.navi;

public class NaviCongestionDetailInfoEntity {
    private int timeOfSeconds;
    private int beginSegmentIndex;
    private int beginLinkIndex;
    private int status;
    private int remainDist;

    public int getTimeOfSeconds() {
        return timeOfSeconds;
    }

    public void setTimeOfSeconds(int timeOfSeconds) {
        this.timeOfSeconds = timeOfSeconds;
    }

    public int getBeginSegmentIndex() {
        return beginSegmentIndex;
    }

    public void setBeginSegmentIndex(int beginSegmentIndex) {
        this.beginSegmentIndex = beginSegmentIndex;
    }

    public int getBeginLinkIndex() {
        return beginLinkIndex;
    }

    public void setBeginLinkIndex(int beginLinkIndex) {
        this.beginLinkIndex = beginLinkIndex;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public int getRemainDist() {
        return remainDist;
    }

    public void setRemainDist(int remainDist) {
        this.remainDist = remainDist;
    }

    @Override
    public String toString() {
        return "NaviCongestionDetailInfoEntity{" +
                "timeOfSeconds=" + timeOfSeconds +
                ", beginSegmentIndex=" + beginSegmentIndex +
                ", beginLinkIndex=" + beginLinkIndex +
                ", status=" + status +
                ", remainDist=" + remainDist +
                '}';
    }
}
