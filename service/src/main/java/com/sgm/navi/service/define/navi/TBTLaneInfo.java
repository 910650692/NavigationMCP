package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

/**
 * @Description: 导航车道线
 * @Date: 2024/1/12
 * @author sgm
 * @version $Revision.*$
 */
public class TBTLaneInfo {
    private boolean mIsRecommend = false;
    private int mLaneAction = 0;
    private boolean mIsTimeLane = false;
    private int mTimeLaneAction = 0;

    public boolean isRecommend() {
        return mIsRecommend;
    }

    public void setRecommend(final boolean recommend) {
        mIsRecommend = recommend;
    }

    public int getLaneAction() {
        return mLaneAction;
    }

    public void setLaneAction(final int laneAction) {
        this.mLaneAction = laneAction;
    }

    public boolean isTimeLane() {
        return mIsTimeLane;
    }

    public void setTimeLane(final boolean timeLane) {
        mIsTimeLane = timeLane;
    }

    public int getTimeLaneAction() {
        return mTimeLaneAction;
    }

    public void setTimeLaneAction(final int timeLaneAction) {
        this.mTimeLaneAction = timeLaneAction;
    }

    @NonNull
    @Override
    public String toString() {
        return "TBTLaneInfo{" +
                "isRecommend=" + mIsRecommend +
                ", laneAction=" + mLaneAction +
                ", isTimeLane=" + mIsTimeLane +
                ", timeLaneAction=" + mTimeLaneAction +
                '}';
    }
}

