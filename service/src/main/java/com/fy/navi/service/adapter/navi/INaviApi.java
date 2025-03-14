package com.fy.navi.service.adapter.navi;

import com.fy.navi.service.define.cruise.CruiseParamEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.navi.NaviParamEntity;
import com.fy.navi.service.define.navi.NaviStartType;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface INaviApi {

    void initNaviService();

    void registerObserver(String key, GuidanceObserver guidanceObserver);

    boolean startNavigation(NaviStartType isSimple);

    boolean stopNavigation();

    void setNaviPath(int routeIndex, RouteLineLayerParam routeLineLayerParam);

    void updateNaviPath(int routeIndex, RouteLineLayerParam routeLineLayerParam);

    void unregisterObserver(String key);

    void unInitNaviService();

    long obtainSAPAInfo(boolean isFindRemainPath);

    void selectMainPathID(long pathID);

    void setCruiseParam(CruiseParamEntity cruiseParamEntity);

    void updateGuideParam(NaviParamEntity naviParamEntity);

    void playTRManualExt(int requestId);

    void queryAppointLanesInfo(int segmentIdx, int linkIdx);
}
