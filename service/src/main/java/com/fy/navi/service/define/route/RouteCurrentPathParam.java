package com.fy.navi.service.define.route;

import com.fy.navi.service.define.aos.RestrictedAreaDetail;
import com.fy.navi.service.define.map.MapTypeId;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteCurrentPathParam {
    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;
    private boolean isOnlineRoute = true;
    /*** 详情数据 **/
    private Object pathInfo;
}
