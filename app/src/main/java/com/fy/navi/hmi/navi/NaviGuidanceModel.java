package com.fy.navi.hmi.navi;


import android.os.Bundle;

import com.android.utils.ConvertUtils;


import com.android.utils.NetWorkUtils;
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
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RouteRequestParam;
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
import java.util.concurrent.CopyOnWriteArrayList;

public class NaviGuidanceModel extends BaseModel<NaviGuidanceViewModel> implements IGuidanceObserver,
        ImmersiveStatusScene.IImmersiveStatusCallBack, ISceneCallback, IRouteResultObserver, NetWorkUtils.NetworkObserver {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private final RoutePackage mRoutePackage;
    private final LayerPackage mLayerPackage;
    private final MapPackage mMapPackage;
    private ImersiveStatus mCurrentStatus = ImersiveStatus.IMERSIVE;
    private List<NaviViaEntity> mViaList = new ArrayList<>();
    private NaviEtaInfo mNaviEtaInfo;

    public static final int BRIDGE_SWITCH = 1;
    public static final int ROAD_SWITCH = 0;
    public static final String MAIN_ROAD = "MAIN";
    public static final String SIDE_ROAD = "SIDE";
    public static final String BRIDGE_ON = "ON";
    public static final String BRIDGE_UNDER = "UNDER";
    public static final int ENTER_PREVIEW = 1;
    public static final int EXIT_PREVIEW = 0;
    private NetWorkUtils mNetWorkUtils;
    private List<OnNetStatusChangeListener> mNetStatusChangeListeners =
            new CopyOnWriteArrayList<>();


    public NaviGuidanceModel() {
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mNetWorkUtils = NetWorkUtils.Companion.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        ImmersiveStatusScene.getInstance().registerCallback("NaviGuidanceModel", this);
        mNaviPackage.registerObserver(NaviConstant.KEY_NAVI_MODEL, this);
        mRoutePackage.registerRouteObserver(NaviConstant.KEY_NAVI_MODEL, this);
    }

    @Override
    public void onStart() {
        super.onStart();
    }

    /**
     * 开始导航
     *
     * @param bundle bundle
     */
    public void startNavigation(final Bundle bundle) {
        final boolean isNaviSuccess;
        if (bundle != null) {
            final int anInt = bundle.getInt(
                    AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM,
                    AutoMapConstant.NaviType.NAVI_GPS);
            isNaviSuccess = mNaviPackage.startNavigation(
                    anInt == AutoMapConstant.NaviType.NAVI_SIMULATE);
        } else {
            isNaviSuccess = mNaviPackage.startNavigation(false);
        }
        if (isNaviSuccess) {
            final MapTypeId mapTypeId = MapTypeManager.getInstance().
                    getMapTypeIdByName(mViewModel.mScreenId);
            mNaviPackage.addNaviRecord(false);
            mMapPackage.goToCarPosition(mapTypeId);
            mLayerPackage.setFollowMode(mapTypeId, true);
        }
    }

    @Override
    public void onAttachViewModel(final NaviGuidanceViewModel baseViewModel) {
        super.onAttachViewModel(baseViewModel);
        mViewModel.addSceneCallback(this);
    }

    @Override
    public void onNaviInfo(final NaviEtaInfo naviInfoBean) {
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
    public void onNaviArrive(final long traceId, final int naviType) {
        mViewModel.onNaviArrive(traceId, naviType);
    }

    @Override
    public void onManeuverInfo(final NaviManeuverInfo info) {
        mViewModel.onManeuverInfo(info);
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        Logger.i(TAG, "onUpdateTMCLightBar naviTmcInfo = " + naviTmcInfo.toString());
        mNaviPackage.setTmcData(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo);
    }

    @Override
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        mViewModel.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    @Override
    public void onNaviSpeedOverallInfo(final SpeedOverallEntity speedCameraInfo) {
        mViewModel.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    @Override
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        mViewModel.onNaviSAPAInfo(sapaInfoEntity);
    }

    @Override
    public void onUpdateViaPass(final long viaIndex) {
        mViewModel.onUpdateViaPass(viaIndex);
    }

    @Override
    public void onSelectMainPathStatus(final long pathID, final int result) {
        if (result == NaviConstant.ChangeNaviPathResult.CHANGE_NAVI_PATH_RESULT_SUCCESS) {
            //此处选中路线索引需要从onNotifyClick获取 BizRouteType.BizRouteTypePath/BizRouteType.BizRouteTypeGuideLabel
//            mLayerPackage.switchSelectedPath(MapTypeId.MAIN_SCREEN_MAIN_MAP, pathID);
            //清除引导路线上的转向图标图层
//            mLayerPackage.clearAllItems(MapTypeId.MAIN_SCREEN_MAIN_MAP, NaviConstant.BizRouteType.BizRouteTypeArrow);
//            showRouteSegmentArrow(mCurNaviInfo.curSegIdx);
        }
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        mViewModel.onLaneInfo(isShowLane, laneInfo);
    }

    @Override
    public void onRouteFail(final MapTypeId mapTypeId, final String errorMsg) {
        Logger.i(TAG, "onRouteFail");
    }

    @Override
    public void onRouteResult(final RequestRouteResult requestRouteResult) {
        Logger.i(TAG, "onRouteResult");
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapTypeId.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
    }

    @Override
    public void onDriveReport(final NaviDriveReportEntity naviDriveReportEntity) {
        Logger.i(TAG, "onDriveReport " + naviDriveReportEntity.toString());
        mViewModel.onDriveReport(naviDriveReportEntity);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        NaviSceneManager.getInstance().removeAllBaseScene();
    }

    /**
     * 显示路段转向箭头
     *
     * @param segmentsId 路段ID
     */
    public void showRouteSegmentArrow(final long segmentsId) {
        final ArrayList<Long> data = new ArrayList<>();
        data.add(segmentsId);
        mLayerPackage.setPathArrowSegment(MapTypeManager.getInstance().
                getMapTypeIdByName(mViewModel.mScreenId), data);
        mLayerPackage.updatePathArrow(MapTypeManager.getInstance().
                getMapTypeIdByName(mViewModel.mScreenId));
    }

    @Override
    public void onImmersiveStatusChange(final MapTypeId mapTypeId,
                                        final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "NaviGuidanceModel currentImersiveStatus：" + currentImersiveStatus +
                "，mCurrentStatus：" + mCurrentStatus);
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
        Logger.i(TAG, "skipAlongWayFragment");
        addFragment(new MainAlongWaySearchFragment(), null);
    }

    @Override
    public void skipSettingFragment() {
        Logger.i(TAG, "skipSettingFragment");
        addFragment(new SettingFragment(), null);
    }

    @Override
    public void deleteViaPoint(final NaviViaEntity entity) {
        ISceneCallback.super.deleteViaPoint(entity);
        final PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setPoint(entity.getRealPos());
        poiInfo.setPid(entity.getPid());
        final boolean result = mRoutePackage.removeVia(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), poiInfo, true);
        mViewModel.notifyDeleteViaPointResult(result, entity);
    }

    @Override
    public void updateSceneVisible(final NaviSceneId sceneType, final boolean isVisible) {
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


    /**
     * @return 返回的是一个包含所有的via点的列表
     */
    public List<NaviViaEntity> getViaList() {
        mViaList.clear();
        //[0]代表起点 [size-1]代表终点
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(
                MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        Logger.i(TAG, "allPoiParamList allPoiParamList:" + allPoiParamList.size());
        for (int i = 0; i < allPoiParamList.size(); i++) {
            if (i > 0) {
                final RouteParam routeParam = allPoiParamList.get(i);
                if (i == allPoiParamList.size() - 1) {
                    mViaList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, mNaviEtaInfo));
                } else {
                    final ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = mNaviEtaInfo.viaRemain;
                    Logger.i(TAG, "allPoiParamList viaRemain:" + viaRemain.size());
                    if (!ConvertUtils.isEmpty(viaRemain)) {
                        final int index = i - 1;
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

    /**
     * 路由偏好改变回调
     */
    public void onRoutePreferenceChange() {
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        param.setMRouteWay(RouteWayID.ROUTE_WAY_CHANGE_PREFERENCE);
        mRoutePackage.requestRoute(param);
    }

    /**
     * 语音打开/关闭引导中路线全览.
     *
     * @param mapTypeId mapTypeId，对应底图.
     * @param open true-开启全览  false-关闭全览.
     */
    @Override
    public void onVoiceOverview(final MapTypeId mapTypeId, final boolean open) {
        Logger.i(TAG, "onVoiceOverview open:" + open);
        if (open) {
            mViewModel.naviPreviewSwitch(ENTER_PREVIEW);
        } else {
            mViewModel.naviPreviewSwitch(EXIT_PREVIEW);
        }

    }

    /**
     * 语音切换主辅路、桥上下.
     *
     * @param mapTypeId MapTypeId，对应底图.
     * @param parallelOption ，切换类型，MAIN-主路 SIDE-辅路  ON-桥上  UNDER-桥下.
     */
    @Override
    public void onVoiceParallelOption(final MapTypeId mapTypeId, final String parallelOption) {
        Logger.i(TAG, "onVoiceParallelOption parallelOption:" + parallelOption);
        if (MAIN_ROAD.equals(parallelOption) || SIDE_ROAD.equals(parallelOption)) {
            mViewModel.naviParallelSwitch(ROAD_SWITCH);
        } else {
            mViewModel.naviParallelSwitch(BRIDGE_SWITCH);
        }
    }

    /**
     * 语音继续导航指令.
     *
     * @param mapTypeId MapTypeId，对应底图.
     */
    @Override
    public void onVoiceContinueNavigation(final MapTypeId mapTypeId) {
        Logger.i(TAG, "onVoiceContinueNavigation");
        mViewModel.naviContinue();
    }

    @Override
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        Logger.i(TAG, "skipNaviSapaDetailScene type:" + type + " sapaInfoEntity:" +
                sapaInfoEntity.toString());
        ISceneCallback.super.skipNaviSapaDetailScene(type, sapaInfoEntity);
        mViewModel.skipNaviSapaDetailScene(type, sapaInfoEntity);
    }

    /**
     * 将沉浸态状态信息存储到NaviPackage中方便外部接口调用
     *
     * @param imersiveStatus ImmersiveStatus
     */
    private void setImmersiveStatus(final ImersiveStatus imersiveStatus) {
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

    /**
     * @return true 网络可用，false 网络不可用
     */
    public boolean getNetStatus() {
        // 检查网络状态
        Boolean isNetConnected = mNetWorkUtils.checkNetwork();
        Logger.i(TAG, "getNetStatus isNetConnected:" + isNetConnected);
        return Boolean.TRUE.equals(isNetConnected);
    }

    public interface OnNetStatusChangeListener {
        /**
         * @param isConnected true 网络可用，false 网络不可用
         */
        void onNetStatusChange(boolean isConnected);
    }

    /**
     * @param listener OnNetStatusChangeListener
     */
    public void addOnNetStatusChangeListener(final OnNetStatusChangeListener listener) {
        Logger.i(TAG, "addOnNetStatusChangeListener listener = " + listener);
        if (listener != null && !mNetStatusChangeListeners.contains(listener)) {
            mNetStatusChangeListeners.add(listener);
        }
    }

    /**
     * @param listener OnNetStatusChangeListener
     */
    public void removeOnNetStatusChangeListener(final OnNetStatusChangeListener listener) {
        Logger.i(TAG, "removeOnNetStatusChangeListener listener = " + listener);
        mNetStatusChangeListeners.remove(listener);
    }

    /**
     * 网络状态改变
     */
    private void onNetStatusChange() {
        Boolean isNetConnected = mNetWorkUtils.checkNetwork();
        Logger.i(TAG, "onNetStatusChange isNetConnected:" + isNetConnected);
        if (!ConvertUtils.isEmpty(mNetStatusChangeListeners)) {
            for (OnNetStatusChangeListener listener : mNetStatusChangeListeners) {
                listener.onNetStatusChange(Boolean.TRUE.equals(isNetConnected));
            }
        }
    }

    @Override
    public void onNetConnectSuccess() {
        onNetStatusChange();
    }

    @Override
    public void onNetUnavailable() {
        onNetStatusChange();
    }

    @Override
    public void onNetBlockedStatusChanged() {
        onNetStatusChange();
    }

    @Override
    public void onNetLosing() {
        onNetStatusChange();
    }

    @Override
    public void onNetLinkPropertiesChanged() {
        onNetStatusChange();
    }

    @Override
    public void onNetDisConnect() {
        onNetStatusChange();
    }
}
