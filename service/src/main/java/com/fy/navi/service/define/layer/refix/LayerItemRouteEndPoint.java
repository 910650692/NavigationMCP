package com.fy.navi.service.define.layer.refix;

import androidx.annotation.IntDef;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import lombok.Getter;
import lombok.Setter;

/**
 * BizCruiseCongestionInfo
 */
@Setter
@Getter
public class LayerItemRouteEndPoint extends LayerItemData {

    @IRouteEndPointType
    private int endPointType;   //终点扎标类型
    private int restNum;    //剩余油量/电量

    @Retention(RetentionPolicy.CLASS)
    @IntDef({IRouteEndPointType.LAYER_ROUTE_END_TYPE_DEFAULT, IRouteEndPointType.LAYER_ROUTE_END_TYPE_BATTERY,
            IRouteEndPointType.LAYER_ROUTE_END_TYPE_OIL})
    public @interface IRouteEndPointType {
        /**
         * 默认终点扎标
         */
        int LAYER_ROUTE_END_TYPE_DEFAULT = 0;
        /**
         * 剩余电量
         */
        int LAYER_ROUTE_END_TYPE_BATTERY = 1;
        /**
         * 剩余油量
         */
        int LAYER_ROUTE_END_TYPE_OIL = 2;
    }

    @Override
    public String toString() {
        return "LayerItemRouteEndPoint{" +
                "endPointType=" + endPointType +
                ", restNum=" + restNum +
                '}';
    }
}
