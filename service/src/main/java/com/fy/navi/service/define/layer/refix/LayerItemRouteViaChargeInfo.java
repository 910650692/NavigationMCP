package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.define.route.RouteChargeStationInfo;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * 路线图层补能规划数据
 */
@Setter
@Getter
public class LayerItemRouteViaChargeInfo extends LayerItemData {

    private RouteChargeStationDetailInfo mRouteChargeStationInfo;
    private int index;

    @Override
    public String toString() {
        return "LayerItemRouteViaChargeInfo{" +
                "mRouteChargeStationInfo=" + mRouteChargeStationInfo +
                ", index=" + index +
                '}';
    }
}
