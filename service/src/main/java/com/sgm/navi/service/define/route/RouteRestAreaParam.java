package com.sgm.navi.service.define.route;

import com.sgm.navi.service.define.map.MapType;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteRestAreaParam {

    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapType mMapTypeId;
    private boolean mIsOnlineRoute = true;
    /*** 服务区信息 **/
    private List<RouteRestAreaInfo> mRouteRestAreaInfos;
    /*** 服务区扎点使用 **/
    private ArrayList<?> mPathInfoList = new ArrayList<>();
}
