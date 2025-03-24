package com.fy.navi.service.adapter.l2;

import com.fy.navi.service.define.navi.L2NaviBean;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
public interface L2DriveObserver {
    void onNaviStatus(String naviStatus);

    void onSelectRouteIndex(String routeId);

    void onParkingInfo(String parkInfo);

    void onNaviInfo(String l2NaviBean);
}
