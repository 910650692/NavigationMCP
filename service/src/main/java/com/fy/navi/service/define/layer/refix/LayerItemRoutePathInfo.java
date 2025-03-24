package com.fy.navi.service.define.layer.refix;


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


    public ArrayList<LayerItemRoutePoint> mStartPoints = new ArrayList<>();
    public ArrayList<LayerItemRoutePoint> mEndPoints = new ArrayList<>();
    public ArrayList<LayerItemRoutePoint> mViaPoints = new ArrayList<>();
}
