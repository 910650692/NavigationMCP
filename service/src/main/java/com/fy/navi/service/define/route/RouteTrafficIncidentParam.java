package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapType;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
@Getter
@Setter
public class RouteTrafficIncidentParam {
    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapType mMapTypeId;
    private boolean mIsOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteTrafficIncidentInfo> mRouteTrafficIncidentInfos;
}
