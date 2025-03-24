package com.fy.navi.service.define.route;

import com.fy.navi.service.define.search.PoiInfoEntity;

import java.io.Serializable;
import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteMsgPushInfo implements Serializable {
    private String mName;
    private Object mMsgPushInfo;
    private PoiInfoEntity mPoiInfoEntity;
    private RoutePoint mStartPoint;
    private RoutePoint mEndPoint;
    private ArrayList<RoutePoint> mViaPoints = new ArrayList<>();
    private ArrayList<PoiInfoEntity> mViaPoiInfoEntity = new ArrayList<>();
}
