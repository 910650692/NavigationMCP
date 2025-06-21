package com.fy.navi.service.define.layer;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteLinePoints;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteLineLayerParam {
    private long mRequestId;
    private MapType mMapTypeId;
    private int mRouteType;
    private int mStrategy;
    private boolean mIsOnlineRoute = true;
    private Object mPoiForRequest;
    /*** 底图上的路线图层信息 **/
    private ArrayList<?> mPathInfoList = new ArrayList<>();
    private RouteLinePoints mRouteLinePoints = new RouteLinePoints();
    private boolean mPassGrey = true;
    private int mSelectIndex = 0;
    /*** 是否绘制路线图层 **/
    private boolean mIsDrawLineLayer = true;

    @NonNull
    @Override
    public String toString() {
        return "RouteLineLayerParam{" +
                "requestId=" + mRequestId +
                ", mapTypeId=" + mMapTypeId +
                ", pathInfoList=" + mPathInfoList +
                ", routeLinePoints=" + mRouteLinePoints +
                ", passGrey=" + mPassGrey +
                ", selectIndex=" + mSelectIndex +
                '}';
    }
}
