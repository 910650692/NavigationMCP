package com.sgm.navi.service.define.navi;

import java.util.HashMap;

import lombok.Getter;

public class RoadName {
    // 道路唯一标识
    private long mPathId;
    // String为道路名称，Integer为道路的LinkIndex，LinkIndex用于获取对应道路的Tmc状态
    private HashMap<String, RoadLink> mRoadNameMap;

    public HashMap<String, RoadLink> getRoadNameMap() {
        return mRoadNameMap;
    }

    public void setRoadNameMap(final HashMap<String, RoadLink> roadNameAndTmc) {
        this.mRoadNameMap = roadNameAndTmc;
    }

    public long getPathId() {
        return mPathId;
    }

    public void setPathId(final long pathId) {
        this.mPathId = pathId;
    }

    @Getter
    public static class RoadLink {
        int length;
        int linkIndex;

        public RoadLink(int length, int linkIndex) {
            this.length = length;
            this.linkIndex = linkIndex;
        }

    }
}
