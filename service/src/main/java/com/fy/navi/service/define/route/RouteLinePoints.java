package com.fy.navi.service.define.route;

import androidx.annotation.NonNull;

import java.util.ArrayList;

/**
 * @Description 绘制路线的起终点和途径点参数
 * @Author lww
 * @date 2024/12/10
 */
public class RouteLinePoints {
    /*** 路线起点 **/
    private ArrayList<RoutePoint> mStartPoints = new ArrayList<>();
    /*** 路线途径点 **/
    private ArrayList<RoutePoint> mViaPoints = new ArrayList<>();
    /*** 路线终点**/
    private ArrayList<RoutePoint> mEndPoints = new ArrayList<>();

    public ArrayList<RoutePoint> getStartPoints() {
        return mStartPoints;
    }

    public void setStartPoints(ArrayList<RoutePoint> mStartPoints) {
        this.mStartPoints = mStartPoints;
    }

    public ArrayList<RoutePoint> getEndPoints() {
        return mEndPoints;
    }

    public void setEndPoints(ArrayList<RoutePoint> mEndPoints) {
        this.mEndPoints = mEndPoints;
    }

    public ArrayList<RoutePoint> getViaPoints() {
        return mViaPoints;
    }

    public void setViaPoints(ArrayList<RoutePoint> mViaPoints) {
        this.mViaPoints = mViaPoints;
    }

    @NonNull
    @Override
    public String toString() {
        return "RouteLinePoints{" +
                "mStartPoints=" + mStartPoints +
                ", mViaPoints=" + mViaPoints +
                ", mEndPoints=" + mEndPoints +
                '}';
    }
}
