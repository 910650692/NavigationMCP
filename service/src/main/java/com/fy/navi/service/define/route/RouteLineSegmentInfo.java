package com.fy.navi.service.define.route;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteLineSegmentInfo {
    private int mIconType;
    private String mLoadName;
    private String mDistance;
    private long mLightCount;
    private List<Long> mAvoidList = new ArrayList<>();
    private List<RouteLineSegmentInfo> mRouteLineSegmentInfos = new ArrayList<>();
}
