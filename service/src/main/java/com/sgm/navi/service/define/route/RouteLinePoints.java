package com.sgm.navi.service.define.route;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteLinePoints {
    /*** 路线起点 **/
    private ArrayList<RoutePoint> mStartPoints = new ArrayList<>();
    /*** 路线途径点 **/
    private ArrayList<RoutePoint> mViaPoints = new ArrayList<>();
    /*** 路线终点**/
    private ArrayList<RoutePoint> mEndPoints = new ArrayList<>();
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
