package com.fy.navi.service.define.route;


import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteRestAreaInfo {
    private List<RouteRestAreaDetailsInfo> mRouteRestAreaDetailsInfos = new ArrayList<>();
}
