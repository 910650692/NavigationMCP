package com.fy.navi.mapservice.common;

import com.fy.navi.mapservice.common.INaviAutoApiCallback;
import com.fy.navi.mapservice.bean.common.BaseGeoPoint;
import com.fy.navi.mapservice.bean.common.BaseSearchPoi;
import com.fy.navi.mapservice.bean.common.BaseTurnInfo;

interface INaviAutoApiBinder {

    void addNaviAutoApiCallback(String pkgName, INaviAutoApiCallback naviAutoApiCallback);

    void removeNaviAutoApiCallback(String pkgName, INaviAutoApiCallback naviAutoApiCallback);

    void openMap(String pkgName);

    String getCurrentLocation(String pkgName);

    String getDistrictDetailInfo(String pkgName);

    void jumpToSearchPage(in String pkgName, in String keyword);

    int requestReverseGeoSearch(in String pkgName, in BaseGeoPoint geoPoint);

    void nearbySearch(String pkgName, String keyword, int pageIndex);

    void searchAndNavi(String pkgName, in String address);

    void cancelAllSearchRequest(String pkgName);

    void routePlan(String pkgName, in BaseSearchPoi destPoint);

    boolean isNaviStatus(String pkgName);

    void startNavi(String pkgName);

    int getGuidePanelStatus(String pkgName);

    String getTBTInfo(String pkgName);

    String getNaviType(String pkgName);


}