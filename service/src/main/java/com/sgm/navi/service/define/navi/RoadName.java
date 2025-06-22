package com.sgm.navi.service.define.navi;

import java.util.HashMap;

public class RoadName {
    // 道路唯一标识
    private long mPathId;
    // String为道路名称，Integer为道路的LinkIndex，LinkIndex用于获取对应道路的Tmc状态
    private HashMap<String, Integer> mRoadNameMap;

    public HashMap<String, Integer> getRoadNameMap() {
        return mRoadNameMap;
    }

    public void setRoadNameMap(final HashMap<String, Integer> roadNameAndTmc) {
        this.mRoadNameMap = roadNameAndTmc;
    }

    public long getPathId() {
        return mPathId;
    }

    public void setPathId(final long pathId) {
        this.mPathId = pathId;
    }
}
