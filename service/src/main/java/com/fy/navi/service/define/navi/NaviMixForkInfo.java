package com.fy.navi.service.define.navi;


import com.fy.navi.service.define.bean.GeoPoint;

import lombok.Getter;
import lombok.Setter;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/14
 */
@Getter
@Setter
public class NaviMixForkInfo {
    public GeoPoint pos;
    public int dist;
    public int roadclass;
    public int segmentIndex;

    public NaviMixForkInfo(GeoPoint pos, int dist, int roadclass, int segmentIndex) {
        this.pos = pos;
        this.dist = dist;
        this.roadclass = roadclass;
        this.segmentIndex = segmentIndex;
    }

    @Override
    public String toString() {
        return "NaviMixForkInfo{" +
                "pos=" + pos +
                ", dist=" + dist +
                ", roadclass=" + roadclass +
                ", segmentIndex=" + segmentIndex +
                '}';
    }
}
