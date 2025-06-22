package com.sgm.navi.service.define.layer.refix;

import com.sgm.navi.service.define.route.RouteChargeStationDetailInfo;

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
