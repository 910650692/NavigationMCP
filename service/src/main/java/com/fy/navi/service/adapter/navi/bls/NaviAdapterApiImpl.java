package com.fy.navi.service.adapter.navi.bls;

import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.guide.model.NaviPath;
import com.autonavi.gbl.guide.model.NaviType;
import com.autonavi.gbl.guide.model.QueryLanesInfo;
import com.autonavi.gbl.guide.model.guidecontrol.Param;
import com.autonavi.gbl.guide.model.guidecontrol.Type;
import com.autonavi.gbl.guide.model.guidecontrol.ElecVehicleCharge;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.adapter.navi.INaviApi;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.cruise.CruiseParamEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.navi.NaviParamEntity;
import com.fy.navi.service.define.navi.NaviStartType;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.utils.BevPowerCarUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 导航接口的第一层封装
 *
 * @author fy
 * @version $Revision.*$
 */
public class NaviAdapterApiImpl extends BaseGuideAdapterApiImpl implements INaviApi {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private final NaviApiImplHelper mNaviApiImplHelper;
    //引导id,唯一标识
    private long mNaviId;

    public NaviAdapterApiImpl() {
        super();
        mNaviApiImplHelper = new NaviApiImplHelper(getGuideService());
    }

    @Override
    public void initNaviService() {
        mNaviApiImplHelper.initNaviService();
        Logger.d(TAG, "NaviAdapterApiImpl initNaviService: ");
    }

    @Override
    public void registerObserver(final String key, final GuidanceObserver guidanceObserver) {
        mNaviApiImplHelper.registerObserver(key, guidanceObserver);
    }

    @Override
    public boolean startNavigation(final NaviStartType naviStartType) {
        final NaviPath naviPath = mNaviApiImplHelper.getNaviPathParam();
        final ArrayList<PathInfo> vecPaths = naviPath.vecPaths;
        boolean startNaviSuccess = false;
        if (!vecPaths.isEmpty()) {
            mNaviApiImplHelper.initGuideParam();
            final boolean setNaviPathSuccess = getGuideService().setNaviPath(naviPath);
            Logger.i(TAG, "NaviAdapterApiImpl setNaviPath: " + setNaviPathSuccess);
            if (naviStartType == NaviStartType.NAVI_TYPE_GPS) {
                mNaviId = NaviConstant.NAVI_ID;
                startNaviSuccess = getGuideService().startNavi(mNaviId, NaviType.NaviTypeGPS);
            } else {
                mNaviId = NaviConstant.NAVI_SIM_ID;

                startNaviSuccess = getGuideService().startNavi(mNaviId, NaviType.NaviTypeSimulation);
            }
            Logger.i(TAG, "NaviAdapterApiImpl startNavi: " + startNaviSuccess + ",mNaviId：" + mNaviId);
        } else {
            Logger.e(TAG, "NaviAdapterApiImpl startNavi: vecPaths.isEmpty");
        }
        return startNaviSuccess;
    }

    @Override
    public void updateNaviPath(final int routeIndex,
                               final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "updateNaviPath: ");
        mNaviApiImplHelper.setNaviPathParam(routeIndex, routeLineLayerParam);
        final NaviPath naviPath = mNaviApiImplHelper.getNaviPathParam();
        getGuideService().setNaviPath(naviPath);
    }

    @Override
    public boolean stopNavigation() {
        final boolean b = getGuideService().stopNavi(mNaviId);
        Logger.i(TAG, "NaviAdapterApiImpl stopNavigation: " + mNaviId + ",stopNavi：" + b);
        return b;
    }

    @Override
    public void setNaviPath(final int routeIndex, final RouteLineLayerParam routeLineLayerParam) {
        mNaviApiImplHelper.setNaviPathParam(routeIndex, routeLineLayerParam);
    }

    @Override
    public void unregisterObserver(final String key) {
        mNaviApiImplHelper.unregisterObserver(key);
    }

    @Override
    public void unInitNaviService() {
        mNaviApiImplHelper.unit();
        getGuideService().unInit();
    }

    @Override
    public long obtainSAPAInfo(final boolean isFindRemainPath) {
        return mNaviApiImplHelper.obtainSAPAInfo(isFindRemainPath);
    }

    @Override
    public void selectMainPathID(final long pathID) {
        mNaviApiImplHelper.selectMainPathID(pathID);
    }

    @Override
    public void setCruiseParam(final CruiseParamEntity cruiseParamEntity) {
        mNaviApiImplHelper.setCruiseParam(cruiseParamEntity);
    }

    @Override
    public void updateGuideParam(final NaviParamEntity naviParamEntity) {
        mNaviApiImplHelper.updateGuideParam(naviParamEntity);
    }

    @Override
    public void playTRManualExt(final int requestId) {
        Logger.i(TAG, "playTRManualExt: ");
        mNaviApiImplHelper.playTRManualExt(requestId);
    }

    @Override
    public void queryAppointLanesInfo(final int segmentIdx, final int linkIdx) {
        final QueryLanesInfo queryLanesInfo = new QueryLanesInfo(segmentIdx, linkIdx, -1);
        getGuideService().queryAppointLanesInfo(queryLanesInfo);
    }

    @Override
    public void getTunnelLength() {
        // TODO
//        NaviPath naviPath = naviApiImplHelper.getNaviPathParam();
    }

    @Override
    public void updateBatteryInfo() {
        final float currentVehicleCharge = BevPowerCarUtils.getInstance().initlialHVBattenergy;
        final Param param = new Param();
        param.type = Type.GuideParamElecVehicleCharge; // 代表电量更新
        final ElecVehicleCharge elecVehicleCharge = new ElecVehicleCharge();
        elecVehicleCharge.vehicleCharge = currentVehicleCharge;
        param.elecVehicle = elecVehicleCharge;
        final boolean isSuccess = getGuideService().setParam(param);
        Logger.i(TAG, "updateBatteryInfo:" + isSuccess, "currentVehicleCharge:" +
                currentVehicleCharge);
    }

    @Override
    public List<NaviViaEntity> getAllViaPoints(PathInfo pathInfo) {
        return mNaviApiImplHelper.getAllViaPoints(pathInfo);
    }

    @Override
    public void pauseNavi() {
        mNaviApiImplHelper.pauseNavi(mNaviId);
    }

    @Override
    public void resumeNavi() {
        mNaviApiImplHelper.resumeNavi(mNaviId);
    }

    @Override
    public void setSimulationSpeed(int simulationSpeed) {
        mNaviApiImplHelper.setSimulationSpeedParam(simulationSpeed);
    }
}
