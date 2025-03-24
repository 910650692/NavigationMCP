package com.fy.navi.service.logicpaket.l2;

public interface L2InfoCallback {

    /**
     * 导航状态监听
     * @param naviStatus 导航状态
     * */
    default void onNaviStatus(String naviStatus){

    }
    /**
     * 选中道路回调
     * @param routeId 道路id
     * */
    default void onSelectRouteIndex(String routeId){

    }
    /**
     * 停车场数据
     * @param parkInfo 停车场数据
     * */
    default void onParkingInfo(String parkInfo){

    }
    /**
     * l2 数据
     * @param l2NaviBean 路数据
     * */
    default void onNaviInfo(String l2NaviBean){

    }
}
