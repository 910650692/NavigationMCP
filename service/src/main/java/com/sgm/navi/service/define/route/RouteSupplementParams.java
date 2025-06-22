package com.sgm.navi.service.define.route;

import java.io.Serializable;
import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteSupplementParams implements Serializable {
    //补能点总时间
    private float mTotalDistance;
    //补能点信息
    private ArrayList<RouteSupplementInfo> mRouteSupplementInfos;
}
