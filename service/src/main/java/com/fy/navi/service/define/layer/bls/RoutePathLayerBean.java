package com.fy.navi.service.define.layer.bls;

/**
 * @Description 路线类型的layer bean.
 * @Author lvww
 * @date 2024/12/20
 */
public class RoutePathLayerBean {
    private RouteTypePoint route_type_start_point;

    public class RouteTypePoint{
        private pointLayerItemStyle point_layer_item_style;
    }

    public class pointLayerItemStyle{
        private NormalStyle normal_style;
    }

    public class NormalStyle{
         private String poi_marker_id;
        private String  poi_marker_info;
    }
}
