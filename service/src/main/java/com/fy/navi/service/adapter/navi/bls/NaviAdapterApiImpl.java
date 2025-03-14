package com.fy.navi.service.adapter.navi.bls;

import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.guide.model.NaviPath;
import com.autonavi.gbl.guide.model.NaviType;
import com.autonavi.gbl.guide.model.QueryLanesInfo;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.adapter.navi.INaviApi;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.cruise.CruiseParamEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.navi.NaviParamEntity;
import com.fy.navi.service.define.navi.NaviStartType;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class NaviAdapterApiImpl extends BaseGuideAdapterApiImpl implements INaviApi {
    private final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private final NaviApiImplHelper naviApiImplHelper;
    //引导id,唯一标识
    private long mNaviId;

    public NaviAdapterApiImpl() {
        super();
        naviApiImplHelper = new NaviApiImplHelper(mGuideService);
    }

    @Override
    public void initNaviService() {
        naviApiImplHelper.initNaviService();
        Logger.d(TAG, "NaviAdapterApiImpl initNaviService: ");
    }

    @Override
    public void registerObserver(String key, GuidanceObserver guidanceObserver) {
        naviApiImplHelper.registerObserver(key, guidanceObserver);
    }

    @Override
    public boolean startNavigation(NaviStartType naviStartType) {
        NaviPath naviPath = naviApiImplHelper.getNaviPathParam();
        ArrayList<PathInfo> vecPaths = naviPath.vecPaths;
        boolean startNaviSuccess = false;
        if (!vecPaths.isEmpty()) {
            naviApiImplHelper.initGuideParam();
            boolean setNaviPathSuccess = mGuideService.setNaviPath(naviPath);
            Logger.i(TAG, "NaviAdapterApiImpl setNaviPath: " + setNaviPathSuccess);
            if (naviStartType == NaviStartType.NAVI_TYPE_GPS) {
                mNaviId = NaviConstant.NAVI_ID;
                startNaviSuccess = mGuideService.startNavi(mNaviId, NaviType.NaviTypeGPS);
            } else {
                mNaviId = NaviConstant.NAVI_SIM_ID;
                startNaviSuccess = mGuideService.startNavi(mNaviId, NaviType.NaviTypeSimulation);
            }
            Logger.i(TAG, "NaviAdapterApiImpl startNavi: " + startNaviSuccess + ",mNaviId：" + mNaviId);
        } else {
            Logger.e(TAG, "NaviAdapterApiImpl startNavi: vecPaths.isEmpty");
        }
        return startNaviSuccess;
    }

    @Override
    public void updateNaviPath(int routeIndex, RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "updateNaviPath: ");
        naviApiImplHelper.setNaviPathParam(routeIndex, routeLineLayerParam);
        NaviPath naviPath = naviApiImplHelper.getNaviPathParam();
        mGuideService.setNaviPath(naviPath);
    }

    @Override
    public boolean stopNavigation() {
        boolean b = mGuideService.stopNavi(mNaviId);
        Logger.i(TAG, "NaviAdapterApiImpl stopNavigation: " + mNaviId + ",stopNavi：" + b);
        return b;
    }

    @Override
    public void setNaviPath(int routeIndex, RouteLineLayerParam routeLineLayerParam) {
        naviApiImplHelper.setNaviPathParam(routeIndex, routeLineLayerParam);
    }

    @Override
    public void unregisterObserver(String key) {
        naviApiImplHelper.unregisterObserver(key);
    }

    @Override
    public void unInitNaviService() {
        naviApiImplHelper.unit();
        mGuideService.unInit();
    }

    @Override
    public long obtainSAPAInfo(boolean isFindRemainPath) {
        return naviApiImplHelper.obtainSAPAInfo(isFindRemainPath);
    }

    @Override
    public void selectMainPathID(long pathID) {
        naviApiImplHelper.selectMainPathID(pathID);
    }

    @Override
    public void setCruiseParam(CruiseParamEntity cruiseParamEntity) {
        naviApiImplHelper.setCruiseParam(cruiseParamEntity);
    }

    @Override
    public void updateGuideParam(NaviParamEntity naviParamEntity) {
        naviApiImplHelper.updateGuideParam(naviParamEntity);
    }

    @Override
    public void playTRManualExt(int requestId) {
        Logger.i(TAG, "playTRManualExt: ");
        naviApiImplHelper.playTRManualExt(requestId);
    }

    @Override
        public void queryAppointLanesInfo(int segmentIdx, int linkIdx) {
        QueryLanesInfo queryLanesInfo = new QueryLanesInfo(segmentIdx, linkIdx, -1);
        mGuideService.queryAppointLanesInfo(queryLanesInfo);
    }
}
