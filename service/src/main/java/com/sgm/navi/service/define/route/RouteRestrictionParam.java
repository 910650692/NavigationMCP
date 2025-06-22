package com.sgm.navi.service.define.route;

import com.sgm.navi.service.define.aos.RestrictedArea;
import com.sgm.navi.service.define.map.MapType;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteRestrictionParam implements Serializable{
    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapType mMapTypeId;
    private boolean mIsOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteRestrictionInfo> mRouteRestrictionInfo = new ArrayList<>();

    private List<String> mRuleIds = new ArrayList<>();

    private Object mReStrictedAreaResponseParam;

    private RestrictedArea mRestrictedArea;
}
