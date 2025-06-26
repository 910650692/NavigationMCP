package com.sgm.navi.service.define.signal;

import java.util.Arrays;

public class RoadConditionGroup {
    private Integer roadSegmentCount;
    private Integer roadSegmentIndex;
    private Integer segmentLength;
    private Integer segmentTime;
    private Integer segmentCondition;
    private Integer remainDistance;
    private Integer remainTime;
    private Integer dataInvalid;
    private final Integer[] signalArray = new Integer[8];

    public Integer[] toArray() {
        signalArray[0] = roadSegmentCount;
        signalArray[1] = roadSegmentIndex;
        signalArray[2] = segmentLength;
        signalArray[3] = segmentTime;
        signalArray[4] = segmentCondition;
        signalArray[5] = remainDistance;
        signalArray[6] = remainTime;
        signalArray[7] = dataInvalid;
        return signalArray;
    }

    public Integer getRoadSegmentCount() {
        return roadSegmentCount;
    }

    public void setRoadSegmentCount(Integer roadSegmentCount) {
        this.roadSegmentCount = roadSegmentCount;
    }

    public Integer getRoadSegmentIndex() {
        return roadSegmentIndex;
    }

    public void setRoadSegmentIndex(Integer roadSegmentIndex) {
        this.roadSegmentIndex = roadSegmentIndex;
    }

    public Integer getSegmentLength() {
        return segmentLength;
    }

    public void setSegmentLength(Integer segmentLength) {
        this.segmentLength = segmentLength;
    }

    public Integer getSegmentTime() {
        return segmentTime;
    }

    public void setSegmentTime(Integer segmentTime) {
        this.segmentTime = segmentTime;
    }

    public Integer getSegmentCondition() {
        return segmentCondition;
    }

    public void setSegmentCondition(Integer segmentCondition) {
        this.segmentCondition = segmentCondition;
    }

    public Integer getRemainDistance() {
        return remainDistance;
    }

    public void setRemainDistance(Integer remainDistance) {
        this.remainDistance = remainDistance;
    }

    public Integer getRemainTime() {
        return remainTime;
    }

    public void setRemainTime(Integer remainTime) {
        this.remainTime = remainTime;
    }

    public Integer getDataInvalid() {
        return dataInvalid;
    }

    public void setDataInvalid(Integer dataInvalid) {
        this.dataInvalid = dataInvalid;
    }

    @Override
    public String toString() {
        return Arrays.toString(signalArray);
    }
}
