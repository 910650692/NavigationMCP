package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteWeatherParam {
    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapTypeId mMapTypeId;
    private boolean mIsOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteWeatherInfo> mRouteWeatherInfos;
    /*** 绘制天气参数 **/
    private ArrayList<?> mWeatherLabelItem = new ArrayList<>();


}
