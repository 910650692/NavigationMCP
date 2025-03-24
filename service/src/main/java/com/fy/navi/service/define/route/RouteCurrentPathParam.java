package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteCurrentPathParam {
    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapTypeId mMapTypeId;
    private boolean mIsOnlineRoute = true;
    /*** 详情数据 **/
    private Object mPathInfo;
}
