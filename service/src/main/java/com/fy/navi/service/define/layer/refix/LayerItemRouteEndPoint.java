package com.fy.navi.service.define.layer.refix;

import lombok.Getter;
import lombok.Setter;

/**
 * 路线图层终点扎标内容
 */
@Setter
@Getter
public class LayerItemRouteEndPoint extends LayerItemData {

    private LayerPointItemType endPointType;   //终点扎标类型
    private int restNum;    //剩余油量/电量
    private String businessHours;   //营业时间

    @Override
    public String toString() {
        return "LayerItemRouteEndPoint{" +
                "endPointType=" + endPointType +
                ", restNum=" + restNum +
                ", businessHours='" + businessHours + '\'' +
                '}';
    }

}
