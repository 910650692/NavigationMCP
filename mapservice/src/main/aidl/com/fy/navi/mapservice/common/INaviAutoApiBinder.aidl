package com.fy.navi.mapservice.common;

import com.fy.navi.mapservice.common.INaviAutoApiCallback;
import com.fy.navi.mapservice.common.INaviAutoCountDownLightCallback;
import com.fy.navi.mapservice.common.INaviAutoLocationCallback;
import com.fy.navi.mapservice.common.INaviAutoRouteCallback;
import com.fy.navi.mapservice.common.INaviAutoSearchCallback;
import com.fy.navi.mapservice.common.INaviAutoStatusCallback;
import com.fy.navi.mapservice.common.INaviAutoSpeedCallBack;
import com.fy.navi.mapservice.common.INaviAutoPoiCallBack;
import com.fy.navi.mapservice.bean.common.BaseGeoPoint;
import com.fy.navi.mapservice.bean.common.BaseSearchPoi;
import com.fy.navi.mapservice.bean.common.BaseTurnInfo;

interface INaviAutoApiBinder {

    void addNaviAutoApiCallback(String pkgName, INaviAutoApiCallback naviAutoApiCallback);

    void removeNaviAutoApiCallback(String pkgName, INaviAutoApiCallback naviAutoApiCallback);

    void addNaviAutoCountDownLightCallback(String pkgName, INaviAutoCountDownLightCallback naviAutoCountDownLightCallback);

    void removeNaviAutoCountDownLightCallback(String pkgName, INaviAutoCountDownLightCallback naviAutoCountDownLightCallback);

    void addNaviAutoLocationCallback(String pkgName, INaviAutoLocationCallback naviAutoLocationCallback);

    void removeNaviAutoLocationCallback(String pkgName, INaviAutoLocationCallback naviAutoLocationCallback);

    void addNaviAutoRouteCallback(String pkgName, INaviAutoRouteCallback naviAutoRouteCallback);

    void removeNaviAutoRouteCallback(String pkgName, INaviAutoRouteCallback naviAutoRouteCallback);

    void addNaviAutoSearchCallback(String pkgName, INaviAutoSearchCallback naviAutoSearchCallback);

    void removeNaviAutoSearchCallback(String pkgName, INaviAutoSearchCallback naviAutoSearchCallback);

    void addNaviAutoStatusCallback(String pkgName, INaviAutoStatusCallback naviAutoStatusCallback);

    void removeNaviAutoStatusCallback(String pkgName, INaviAutoStatusCallback naviAutoStatusCallback);

    void addNaviAutoSpeedCallBack(String pkgName, INaviAutoSpeedCallBack naviAutoSpeedCallBack);

    void removeNaviAutoSpeedCallBack(String pkgName, INaviAutoSpeedCallBack naviAutoSpeedCallBack);

    void addNaviAutoPoiCallBack(String pkgName, INaviAutoPoiCallBack naviAutoPoiCallBack);

    void removeNaviAutoPoiCallBack(String pkgName, INaviAutoPoiCallBack naviAutoPoiCallBack);

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

    void openSrTbt(String pkgName, boolean open);

    boolean stopNavi(String pkgName);

    void backHome(String pkgName);

    void goCompany(String pkgName);

    void openBasicSearch(String pkgName);

    boolean getNaviBroadcastStatus(String pkgName);

    void toggleNaviBroadcast(String pkgName, boolean open);

    void clickPassBySearch(String pkgName);

}