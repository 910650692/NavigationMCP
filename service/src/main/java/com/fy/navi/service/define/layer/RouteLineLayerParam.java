package com.fy.navi.service.define.layer;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteLinePoints;
import com.fy.navi.service.define.route.RoutePoint;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/10
 */
public class RouteLineLayerParam {
    private long requestId;
    private MapTypeId mapTypeId;
    private boolean isOnlineRoute = true;
    /*** 底图上的路线图层信息 **/
    private ArrayList<?> pathInfoList = new ArrayList<>();

    private RouteLinePoints routeLinePoints = new RouteLinePoints();
    private boolean passGrey = true;
    private int selectIndex = 0;
    /*** 是否绘制路线图层 **/
    private boolean isDrawLineLayer = true;
    /*** 设置终点预计到达时间 **/
    private ArrayList<String> estimatedTimeOfArrival = new ArrayList<>();

    public long getRequestId() {
        return requestId;
    }

    public void setRequestId(long requestId) {
        this.requestId = requestId;
    }

    public MapTypeId getMapTypeId() {
        return mapTypeId;
    }

    public void setMapTypeId(MapTypeId mapTypeId) {
        this.mapTypeId = mapTypeId;
    }


    public ArrayList<?> getPathInfoList() {
        return pathInfoList;
    }

    public void setPathInfoList(ArrayList<?> pathInfoList) {
        this.pathInfoList = pathInfoList;
    }

    public RouteLinePoints getRouteLinePoints() {
        return routeLinePoints;
    }

    public void setRouteLinePoints(RouteLinePoints routeLinePoints) {
        this.routeLinePoints = routeLinePoints;
    }

    public boolean isPassGrey() {
        return passGrey;
    }

    public void setPassGrey(boolean passGrey) {
        this.passGrey = passGrey;
    }

    public int getSelectIndex() {
        return selectIndex;
    }

    public void setSelectIndex(int selectIndex) {
        this.selectIndex = selectIndex;
    }

    public boolean isDrawLineLayer() {
        return isDrawLineLayer;
    }

    public void setDrawLineLayer(boolean drawLineLayer) {
        isDrawLineLayer = drawLineLayer;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }

    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }

    public ArrayList<String> getEstimatedTimeOfArrival() {
        return estimatedTimeOfArrival;
    }

    public void setEstimatedTimeOfArrival(ArrayList<String> estimatedTimeOfArrival) {
        this.estimatedTimeOfArrival = estimatedTimeOfArrival;
    }

    @NonNull
    @Override
    public String toString() {
        return "RouteLineLayerParam{" +
                "requestId=" + requestId +
                ", mapTypeId=" + mapTypeId +
                ", pathInfoList=" + pathInfoList +
                ", routeLinePoints=" + routeLinePoints +
                ", passGrey=" + passGrey +
                ", selectIndex=" + selectIndex +
                ", estimatedTimeOfArrival=" + estimatedTimeOfArrival +
                '}';
    }
}
