package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapType;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteChargeStationParam {
    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapType mMapTypeId;
    /*** 充电站 **/
    private ArrayList<RouteChargeStationInfo> mRouteChargeStationInfos;
    /*** 底图上的路线图层信息 **/
    private ArrayList<?> mPathInfoList = new ArrayList<>();
}
