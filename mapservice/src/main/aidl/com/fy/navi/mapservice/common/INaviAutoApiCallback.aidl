package com.fy.navi.mapservice.common;

interface INaviAutoApiCallback {

    void onNaviStatusChange(String status);

    void onLocationInfoChange(String locationInfo);

    void onDistrictInfoChange(String districInfo);

    void onSearchFailed(boolean silent, int errorCode);

    void onSearchResult(boolean silent, String searchResult);

    void onReverseGeoSearchResult(int taskId, String reverseResult);

    void onRoutePlanFailed(int code, String errorMsg);

    void onRoutePlanResult(String routeResult);

    void onPanelData(int panelDataStatus);

    void onSpeedLimitChange(int curSpeed, int limitSpeed);

    void onTurnInfoChange(String turnInfo);

}