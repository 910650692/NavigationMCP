package com.sgm.navi.service.adapter.l2;

import java.util.List;

public class CleaCanData {
    private long totalLength;
    private long totalTime;
    private List<RoadGroupData> roadGroupDatas;

    public long getTotalLength() {
        return totalLength;
    }

    public void setTotalLength(long totalLength) {
        this.totalLength = totalLength;
    }

    public long getTotalTime() {
        return totalTime;
    }

    public void setTotalTime(long totalTime) {
        this.totalTime = totalTime;
    }

    public List<RoadGroupData> getRoadGroupDatas() {
        return roadGroupDatas;
    }

    public void setRoadGroupDatas(List<RoadGroupData> roadGroupDatas) {
        this.roadGroupDatas = roadGroupDatas;
    }
}
