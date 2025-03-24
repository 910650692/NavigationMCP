package com.fy.navi.service.define.route;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteAlterChargeStationParam {
    /*** 请求Id **/
    private long mRequestId;
    /*** 补充充电站 **/
    private ArrayList<RouteAlterChargeStationInfo> mRouteAlterChargeStationInfos;
}
