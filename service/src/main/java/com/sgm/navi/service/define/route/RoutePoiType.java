package com.sgm.navi.service.define.route;

import androidx.annotation.IntDef;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
public interface RoutePoiType {
    /*** 路线起点 **/
    public static final int ROUTE_POI_TYPE_START = 0;
    /*** 路线途径点 **/
    public static final int ROUTE_POI_TYPE_WAY = 1;
    /*** 路线终点 **/
    public static final int ROUTE_POI_TYPE_END = 2;

    @IntDef({ROUTE_POI_TYPE_START, ROUTE_POI_TYPE_WAY, ROUTE_POI_TYPE_END})
    @Retention(RetentionPolicy.RUNTIME)
    public @interface RoutePoiTypeId {

    }
}
