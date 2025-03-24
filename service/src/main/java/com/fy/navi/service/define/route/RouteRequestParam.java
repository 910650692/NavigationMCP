package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;

import lombok.Getter;
import lombok.Setter;


@Getter
@Setter
public class RouteRequestParam {
    private MapTypeId mMapTypeId = MapTypeId.MAIN_SCREEN_MAIN_MAP; //必填
    private PoiInfoEntity mPoiInfoEntity = null; //有点新增则添加
    @RoutePoiType.RoutePoiTypeId
    private int mRoutePoiType; //有点新增则添加
    private boolean mFastNavi = false;
    private RouteWayID mRouteWay = RouteWayID.ROUTE_WAY_DEFAULT; //默认普通算路
    private RoutePreferenceID mRoutePreferenceID = null; //临时偏好时必填，其余场景修改设置即可
    private boolean mIsOnline = true; //不填
    private int mRoutePriorityType = -1; //算路优先级时必填
}
