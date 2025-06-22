package com.sgm.navi.service.define.navi;


import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;

import lombok.Getter;
import lombok.Setter;

/**
 * @author sgm
 * @version $Revision.*$
 */
@Getter
@Setter
public class NaviMixForkInfo {
    private GeoPoint mPos;
    private int mDist;
    private int mRoadclass;
    private int mSegmentIndex;

    public GeoPoint getPos() {
        return mPos;
    }

    public void setPos(final GeoPoint pos) {
        this.mPos = pos;
    }

    public int getMDist() {
        return mDist;
    }

    public void setMDist(final int dist) {
        this.mDist = dist;
    }

    public int getMRoadclass() {
        return mRoadclass;
    }

    public void setMRoadclass(final int roadclass) {
        this.mRoadclass = roadclass;
    }

    public int getMSegmentIndex() {
        return mSegmentIndex;
    }

    public void setMSegmentIndex(final int segmentIndex) {
        this.mSegmentIndex = segmentIndex;
    }

    public NaviMixForkInfo(final GeoPoint pos, final int dist, final int roadclass,
                           final int segmentIndex) {
        this.mPos = pos;
        this.mDist = dist;
        this.mRoadclass = roadclass;
        this.mSegmentIndex = segmentIndex;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviMixForkInfo{" +
                "pos=" + mPos +
                ", dist=" + mDist +
                ", roadclass=" + mRoadclass +
                ", segmentIndex=" + mSegmentIndex +
                '}';
    }
}
