package com.sgm.navi.service.define.route;

import com.sgm.navi.service.define.map.MapType;

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
    private MapType mMapTypeId;
    private boolean mIsOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteWeatherInfo> mRouteWeatherInfos;
    /*** 绘制天气参数 **/
    private ArrayList<?> mWeatherLabelItem = new ArrayList<>();


}
