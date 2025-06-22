package com.sgm.navi.service.define.layer.refix;

import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;

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
    private int type;   //0:补能点, 1: 充电站途经点

    @Override
    public String toString() {
        return "LayerItemRouteReplaceChargePoint{" +
            "info=" + info +
            ", index=" + index +
            ", type=" + type +
            '}';
    }
}
