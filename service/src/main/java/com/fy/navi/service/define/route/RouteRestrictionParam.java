package com.fy.navi.service.define.route;

import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.map.MapTypeId;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteRestrictionParam implements Serializable{
    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;
    private boolean isOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteRestrictionInfo> routeRestrictionInfo = new ArrayList<>();

    private List<String> ruleIds = new ArrayList<>();

    private Object gReStrictedAreaResponseParam;

    private RestrictedArea restrictedArea;

    public RouteRestrictionParam() {}

    // Copy constructor
    public RouteRestrictionParam(RouteRestrictionParam other) {
        this.requestId = other.getRequestId();
        this.mapTypeId = other.getMapTypeId();
        this.routeRestrictionInfo = other.getRouteRestrictionInfo();
        this.ruleIds = other.getRuleIds();
        this.gReStrictedAreaResponseParam = other.getGReStrictedAreaResponseParam();
        this.restrictedArea = other.getRestrictedArea();
    }

    // Copy method
    public RouteRestrictionParam copy() {
        return new RouteRestrictionParam(this);
    }

    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }
}
