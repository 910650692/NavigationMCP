package com.fy.navi.service.define.route;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteAlternativeChargeDetourInfo implements Serializable {
    private int distance;
    private int time;

    public RouteAlternativeChargeDetourInfo() {
        this.distance = Integer.MAX_VALUE;
        this.time = Integer.MAX_VALUE;
    }

    public RouteAlternativeChargeDetourInfo(final int distanceLiteObj, final int timeLiteObj) {
        this.distance = distanceLiteObj;
        this.time = timeLiteObj;
    }
}
