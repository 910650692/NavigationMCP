// INaviAutoRouteCallback.aidl
package com.sgm.navi.mapservice.common;

interface INaviAutoRouteCallback {
    void onRoutePlanFailed(int code, String errorMsg);

    void onRoutePlanResult(String routeResult);

    void onDestChanged(String destInfo);
}