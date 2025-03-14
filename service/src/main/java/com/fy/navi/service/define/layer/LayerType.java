package com.fy.navi.service.define.layer;

import androidx.annotation.IntDef;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Author: QiuYaWei
 * Date: 2025/2/11
 * Description: []
 */
public final class LayerType {
    public static final int MAP_LAYER = 0;
    public static final int SEARCH_LAYER = 1;
    public static final int ROUTE_LAYER = 2;
    public static final int NAVI_LAYER = 3;
    public static final int CROSS_LAYER = 4;
    public static final int EAGLE_EYE_LAYER = 5;
    public static final int AREA_LAYER = 6;
    public static final int ROAD_FACILITY_LAYER = 7;

    @IntDef({MAP_LAYER, SEARCH_LAYER, ROUTE_LAYER, NAVI_LAYER, CROSS_LAYER, EAGLE_EYE_LAYER, AREA_LAYER, ROAD_FACILITY_LAYER})
    @Retention(RetentionPolicy.RUNTIME)
    public @interface LayerId {

    }
}
