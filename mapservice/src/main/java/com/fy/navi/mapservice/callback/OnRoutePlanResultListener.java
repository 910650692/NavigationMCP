package com.fy.navi.mapservice.callback;

public interface OnRoutePlanResultListener {

    void onRoutePlanError(int errorCode, String errorMsg);

    void onRoutePlanResult(String routeResult);
}
