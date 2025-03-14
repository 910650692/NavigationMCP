package com.fy.navi.hmi.navi;


import android.os.Bundle;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.search.alongway.MainAlongWaySearchFragment;
import com.fy.navi.hmi.setting.SettingFragment;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.List;

public class NaviGuidanceModel extends BaseModel<NaviGuidanceViewModel> implements IGuidanceObserver,
        ImmersiveStatusScene.IImmersiveStatusCallBack, ISceneCallback, IRouteResultObserver {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private final RoutePackage mRoutePackage;
    private final LayerPackage mLayerPackage;
    private final MapPackage mMapPackage;
    private ImersiveStatus mCurrentStatus = ImersiveStatus.IMERSIVE;
    private List<NaviViaEntity> mViaList = new ArrayList<>();
    private NaviEtaInfo mNaviEtaInfo;
    private NaviAdapter mNaviAdapter;


    public NaviGuidanceModel() {
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mNaviAdapter = NaviAdapter.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        ImmersiveStatusScene.getInstance().registerCallback("NaviGuidanceModel", this);
        mNaviPackage.registerObserver(NaviConstant.KEY_NAVI_MODEL, this);
        mRoutePackage.registerRouteObserver(NaviConstant.KEY_NAVI_MODEL, this);
    }

    public void startNavigation(Bundle bundle) {
        boolean isNaviSuccess;
        if (bundle != null) {
            int anInt = bundle.getInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, AutoMapConstant.NaviType.NAVI_GPS);
            isNaviSuccess = mNaviPackage.startNavigation(anInt == AutoMapConstant.NaviType.NAVI_SIMULATE);
        } else {
            isNaviSuccess = mNaviPackage.startNavigation(false);
        }
        if (isNaviSuccess) {
            MapTypeId mapTypeId = MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId);
            mNaviPackage.addNaviRecord(false);
            // 导航开启成功后需要重新设置一下车辆视角，否则可能视角不对
//            mMapPackage.switchMapMode(mapTypeId, mMapPackage.getCurrentMapMode(mapTypeId));
            // 如果设置里面开启了自动比例尺功能，这里需要重新开启一下，否则可能不生效
//            mLayerPackage.openDynamicLevel(mapTypeId, SettingPackage.getInstance().getAutoScale());
            mMapPackage.goToCarPosition(mapTypeId);
            mLayerPackage.setFollowMode(mapTypeId, true);
        }
    }

    @Override
    public void onAttachViewModel(NaviGuidanceViewModel baseViewModel) {
        super.onAttachViewModel(baseViewModel);
        mViewModel.addSceneCallback(this);
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviInfoBean) {
        mNaviEtaInfo = naviInfoBean;
        mViewModel.onNaviInfo(naviInfoBean);
    }

    @Override
    public void onNaviStop() {
        mViewModel.onNaviStop();
        mRoutePackage.removeAllRouteInfo(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        mLayerPackage.setVisibleGuideSignalLight(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), false);
        mNaviPackage.addNaviRecord(true);
    }

    @Override
    public void onNaviArrive(long traceId, int naviType) {
        mViewModel.onNaviArrive(traceId, naviType);
    }

    @Override
    public void onManeuverInfo(NaviManeuverInfo info) {
        mViewModel.onManeuverInfo(info);
    }

    @Override
    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
        mNaviAdapter.setTmcData(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo);
    }

    @Override
    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        mViewModel.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    @Override
    public void onNaviSpeedOverallInfo(SpeedOverallEntity speedCameraInfo) {
        mViewModel.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    @Override
    public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
        mViewModel.onNaviSAPAInfo(sapaInfoEntity);
    }

    @Override
    public void onUpdateViaPass(long viaIndex) {
        mViewModel.onUpdateViaPass(viaIndex);
    }

    @Override
    public void onSelectMainPathStatus(long pathID, int result) {
        if (result == NaviConstant.ChangeNaviPathResult.ChangeNaviPathResultSuccess) {
            //此处选中路线索引需要从onNotifyClick获取 BizRouteType.BizRouteTypePath/BizRouteType.BizRouteTypeGuideLabel
//            mLayerPackage.switchSelectedPath(MapTypeId.MAIN_SCREEN_MAIN_MAP, pathID);
            //清除引导路线上的转向图标图层
//            mLayerPackage.clearAllItems(MapTypeId.MAIN_SCREEN_MAIN_MAP, NaviConstant.BizRouteType.BizRouteTypeArrow);
//            showRouteSegmentArrow(mCurNaviInfo.curSegIdx);
        }
    }

    @Override
    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfo) {
        mViewModel.onLaneInfo(isShowLane, laneInfo);
    }

    @Override
    public void onRouteFail(MapTypeId mapTypeId, String errorMsg) {
        Logger.i(TAG, "onRouteFail");
    }

    @Override
    public void onRouteResult(RequestRouteResult requestRouteResult) {
        Logger.i(TAG, "onRouteResult");

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        NaviSceneManager.getInstance().removeAllBaseScene();
//        mNaviPackage.unregisterObserver(NaviConstant.KEY_NAVI_MODEL);
    }

    public void showRouteSegmentArrow(long segmentsId) {
        ArrayList<Long> data = new ArrayList<>();
        data.add(segmentsId);
        mLayerPackage.setPathArrowSegment(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), data);
        mLayerPackage.updatePathArrow(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
    }

    @Override
    public void onImmersiveStatusChange(MapTypeId mapTypeId, ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "NaviGuidanceModel currentImersiveStatus：" + currentImersiveStatus + "，mCurrentStatus：" + mCurrentStatus);
        if (MapTypeId.MAIN_SCREEN_MAIN_MAP.equals(mapTypeId)) {
            setImmersiveStatus(currentImersiveStatus);
        }
        if (currentImersiveStatus != mCurrentStatus) {
            mCurrentStatus = currentImersiveStatus;
            mViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }

    @Override
    public void skipAlongWayFragment() {
        try {
            addFragment(new MainAlongWaySearchFragment(), null);
        } catch (Exception e) {
            Logger.e(TAG, "NaviGuidanceModel skipAlongWayFragment: Exception occurred", e);
        }
    }

    @Override
    public void skipSettingFragment() {
        try {
            addFragment(new SettingFragment(), null);
        } catch (Exception e) {
            Logger.e(TAG, "NaviGuidanceModel skipSettingFragment: Exception occurred", e);
        }
    }

    @Override
    public void deleteViaPoint(NaviViaEntity entity) {
        ISceneCallback.super.deleteViaPoint(entity);
        PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setPoint(entity.getRealPos());
        poiInfo.setPid(entity.getPid());
        boolean result = mRoutePackage.removeVia(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), poiInfo, true);
        mViewModel.notifyDeleteViaPointResult(result, entity);
    }

    @Override
    public void updateSceneVisible(NaviSceneId sceneType, boolean isVisible) {
        mViewModel.updateSceneVisible(sceneType, isVisible);
    }

    @Override
    public void onUpdateViaPass() {
        mViewModel.onUpdateViaPass(-1);
    }

    @Override
    public void skipNaviPreferenceScene() {
        mViewModel.showNaviPreferenceScene();
    }

    /***获取终点、途径点信息***/
    public List<NaviViaEntity> getViaList() {
        mViaList.clear();
        //[0]代表起点 [size-1]代表终点
        List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        Logger.i(TAG, "allPoiParamList allPoiParamList:" + allPoiParamList.size());
        for (int i = 0; i < allPoiParamList.size(); i++) {
            if (i > 0) {
                RouteParam routeParam = allPoiParamList.get(i);
                if (i == allPoiParamList.size() - 1) {
                    mViaList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, mNaviEtaInfo));
                } else {
                    ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = mNaviEtaInfo.viaRemain;
                    Logger.i(TAG, "allPoiParamList viaRemain:" + viaRemain.size());
                    if (!ConvertUtils.isEmpty(viaRemain)) {
                        int index = i - 1;
                        if (viaRemain.size() > index) {
                            mViaList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, viaRemain.get(index)));
                        }
                    } else {
                        mViaList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, null));
                    }
                }
            }
        }
        return mViaList;
    }

    public void onRoutePreferenceChange() {
        List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            RouteParam routeParam = allPoiParamList.get(allPoiParamList.size() - 1);
            mRoutePackage.requestRoute(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), NaviDataFormatHelper.getPoiInfoEntity(routeParam), routeParam.getPoiType(), true, RouteWayID.ROUTE_WAY_DEFAULT);
        }
    }

    @Override
    public void onVoiceOverview(MapTypeId mapTypeId, boolean open) {
        //todo UI模拟打开或关闭全览
    }

    @Override
    public void onVoiceParallelOption(MapTypeId mapTypeId, String parallelOption) {
        //todo UI模拟点击，切换主辅路、高架上下
    }

    private void setImmersiveStatus(ImersiveStatus imersiveStatus) {
        switch (imersiveStatus) {
            case TOUCH -> {
                NaviPackage.getInstance().setCurrentImmersiveStatus(0);
            }
            case IMERSIVE -> {
                NaviPackage.getInstance().setCurrentImmersiveStatus(1);
            }
            default -> {
                NaviPackage.getInstance().setCurrentImmersiveStatus(-1);
            }
        }
    }

}
