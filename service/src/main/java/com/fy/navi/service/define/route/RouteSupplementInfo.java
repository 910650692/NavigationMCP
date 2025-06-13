package com.fy.navi.service.define.route;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteSupplementInfo implements Serializable {
    //补能点类别
    private int mType;
    //补能点距起点位置
    private int mDistance;
    //补能点距起点位置
    private String mUnitDistance;
    //poiID
    private String mPoiID;
    //补能点名称
    private String mName;
    //预计充电事件
    private int mChargeTime;
    //经纬度
    private Coord2DDouble mShow;
    //补能点信息
    private RouteChargeStationDetailInfo mRouteChargeStationDetailInfo;
    //替换补能点信息
    private RouteAlterChargeStationInfo mRouteAlterChargeStationInfo;
}
