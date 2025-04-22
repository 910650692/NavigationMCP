package com.fy.navi.service.define.layer.refix;

import lombok.Getter;
import lombok.Setter;

/**
 * 路线图层终点扎标内容
 */
@Setter
@Getter
public class LayerItemRouteEndPoint extends LayerItemData {

    private LayerRouteEndPointType endPointType;   //终点扎标类型
    private int restNum;    //剩余油量/电量

    @Override
    public String toString() {
        return "RouteLayerEndPointInfo{" +
                "endPointType=" + endPointType +
                ", restNum=" + restNum +
                '}';
    }

    public enum LayerRouteEndPointType {
        /**
         * 默认终点扎标
         */
        LAYER_ROUTE_END_TYPE_DEFAULT,
        /**
         * 剩余电量
         */
        LAYER_ROUTE_END_TYPE_BATTERY,
        /**
         * 剩余油量
         */
        LAYER_ROUTE_END_TYPE_OIL
    }
}
