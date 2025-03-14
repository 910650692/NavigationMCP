package com.fy.navi.service.define.route;

import java.util.ArrayList;
import java.util.List;

public class RouteLineSegmentInfo {
    private int iconType;
    private String loadName;
    private String distance;
    private long lightCount;
    private List<Long> avoidList = new ArrayList<>();
    private List<RouteLineSegmentInfo> routeLineSegmentInfos = new ArrayList<>();

    public int getIconType() {
        return iconType;
    }

    public void setIconType(int iconType) {
        this.iconType = iconType;
    }

    public String getLoadName() {
        return loadName;
    }

    public void setLoadName(String loadName) {
        this.loadName = loadName;
    }

    public String getDistance() {
        return distance;
    }

    public void setDistance(String distance) {
        this.distance = distance;
    }

    public long getLightCount() {
        return lightCount;
    }

    public void setLightCount(long lightCount) {
        this.lightCount = lightCount;
    }

    public void setRouteLineSegmentInfos(List<RouteLineSegmentInfo> routeLineSegmentInfos) {this.routeLineSegmentInfos = routeLineSegmentInfos;}

    public List<RouteLineSegmentInfo> getRouteLineSegmentInfos() {
        return routeLineSegmentInfos;
    }

    public void setAvoidList(List<Long> avoidList) {
        this.avoidList = avoidList;
    }

    public List<Long> getAvoidList() {
        return avoidList;
    }
}
