// INaviAutoRouteCallback.aidl
package com.fy.navi.mapservice.common;

interface INaviAutoRouteCallback {
    void onRoutePlanFailed(int code, String errorMsg);

    void onRoutePlanResult(String routeResult);
}