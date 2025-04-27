package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;

import lombok.Getter;
import lombok.Setter;

/**
 * 路线图层替换补能点扎标内容
 */
@Setter
@Getter
public class LayerItemRouteReplaceChargePoint extends LayerItemData {

    private RouteAlterChargeStationInfo info;
    private int index;

    @Override
    public String toString() {
        return "LayerItemRouteReplaceChargePoint{" +
                "info=" + info +
                ", index=" + index +
                '}';
    }
}
