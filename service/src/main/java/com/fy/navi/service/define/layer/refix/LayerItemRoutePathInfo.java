package com.fy.navi.service.define.layer.refix;


import com.fy.navi.service.define.route.RoutePoint;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * BizCruiseCongestionInfo
 */
@Setter
@Getter
public class LayerItemRoutePathInfo extends LayerItemBase {
    public ArrayList<?> pathInfoList = new ArrayList<>();


    public ArrayList<RoutePoint> mStartPoints = new ArrayList<>();
    public ArrayList<RoutePoint> mEndPoints = new ArrayList<>();
    public ArrayList<RoutePoint> mViaPoints = new ArrayList<>();
}
