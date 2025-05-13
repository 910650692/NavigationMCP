package com.fy.navi.hmi.cluster.cluster_navi_info;


import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.cluster.ClusterNaviGuidanceViewModel;
import com.fy.navi.hmi.cluster.utils.DeleteChargeStationEventMonitor;
import com.fy.navi.service.define.utils.NaviStatusMonitorUtil;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navi.SuggestChangePathReasonEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.List;

public class ClusterNaviGuidanceModel extends BaseModel<ClusterNaviGuidanceViewModel> implements
        IGuidanceObserver, ImmersiveStatusScene.IImmersiveStatusCallBack, ISceneCallback,
        IRouteResultObserver, ILayerPackageCallBack, DeleteChargeStationEventMonitor.OnDeleteChargeStationListener {
    private static final String TAG = "NaviGuidanceTwoTwoModel";
    //导航相关监听
    private final NaviPackage mNaviPackage;
    //路线相关监听
    private final RoutePackage mRoutePackage;
    //图层
    private final LayerPackage mLayerPackage;
    //地图
    private final MapPackage mMapPackage;
    //IMERSIVE 沉浸式状态     TOUCH触控模式
    private ImersiveStatus mCurrentStatus = ImersiveStatus.IMERSIVE;
    // 是否显示自动添加的充电桩
    private boolean mIsShowAutoAdd = true;


    public ClusterNaviGuidanceModel() {
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
    }

    // 是否显示自动添加的充电桩
    public boolean getIsShowAutoAdd() {
        return mIsShowAutoAdd;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");
        NaviSceneManager.getInstance().onCreateSceneView();
        ImmersiveStatusScene.getInstance().registerCallback(TAG, this);
        //导航监听
        mNaviPackage.registerObserver(TAG, this);
        //路线监听
        mRoutePackage.registerRouteObserver(TAG, this);
        //图层
        mLayerPackage.registerCallBack(MapType.CLUSTER_MAP, this);
        //导航状态
        NaviStatusMonitorUtil.getInstance().addListener(mNaviStatusListener);
        DeleteChargeStationEventMonitor.getInstance().addListener(this);
    }
    /**
     * 导航状态监听器
     */
    NaviStatusMonitorUtil.OnNavigationStatusListener mNaviStatusListener = new NaviStatusMonitorUtil.OnNavigationStatusListener() {
        @Override
        public void onNavigationStarted() {
            // 处理导航开始逻辑
            Logger.d(TAG, "导航开始了");
            mMapPackage.goToCarPosition(MapType.CLUSTER_MAP);
            mLayerPackage.setFollowMode(MapType.CLUSTER_MAP, true);
            mViewModel.startNavigation();
        }

        @Override
        public void onNavigationStopped() {
            // 处理导航结束逻辑
            Logger.d(TAG, "导航结束了");
            mMapPackage.goToCarPosition(MapType.CLUSTER_MAP);
            mLayerPackage.setFollowMode(MapType.CLUSTER_MAP, false);
            mViewModel.onNaviStop();
            mRoutePackage.removeAllRouteInfo(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
            mLayerPackage.setVisibleGuideSignalLight(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), false);
            mRoutePackage.clearRouteLine(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        }
    };
    @Override
    public void onAttachViewModel(final ClusterNaviGuidanceViewModel baseViewModel) {
        super.onAttachViewModel(baseViewModel);
        Logger.d(TAG, "onAttachViewModel");
        //场景
        mViewModel.addSceneCallback(this);
    }
    /**
     * 更改导航信息
     * @param naviInfoBean 导航信息
     */
    @Override
    public void onNaviInfo(final NaviEtaInfo naviInfoBean) {
        if (ConvertUtils.isEmpty(naviInfoBean)) return;
        Logger.d(TAG, "onNaviInfo naviInfoBean = " + naviInfoBean.toString());
        //更新导航信息
        mViewModel.onNaviInfo(naviInfoBean);
        //路况大图进度
        int dist = naviInfoBean.NaviInfoData.get(naviInfoBean.NaviInfoFlag).segmentRemain.dist;
        mViewModel.onCrossProgress(dist);
    }
    /**
     * 导航到达目的地
     */
    @Override
    public void onNaviArrive(final long traceId, final int naviType) {
        Logger.d(TAG, "onNaviArrive traceId = " + traceId + " naviType = " + naviType);
        mViewModel.onNaviArrive(traceId, naviType);
    }
    /**
     * 转向图标信息
     */
    @Override
    public void onManeuverInfo(final NaviManeuverInfo info) {
        Logger.d(TAG, "onManeuverInfo info = " + info.toString());
        mViewModel.onManeuverInfo(info);
    }
    /**
     * 更新路况条信息
     */
    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        Logger.d(TAG, "onUpdateTMCLightBar naviTmcInfo = " + naviTmcInfo.toString());
        mNaviPackage.setTmcData(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo, mIsShowAutoAdd);
    }
    /**
     *  路口大图信息
     *  isShowImage 是否显示大图
     *  naviImageInfo 路口大图信息
     */
    @Override
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        mViewModel.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    /**
     * speedCameraInfo 区间车速、绿波车速
     */
    @Override
    public void onNaviSpeedOverallInfo(final SpeedOverallEntity speedCameraInfo) {
        mViewModel.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    /**
     * 服务区、收费站信息、收费口车道类型信息
     */
    @Override
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        mViewModel.onNaviSAPAInfo(sapaInfoEntity);
    }

    /**
     * 更新途经点
     */
    @Override
    public void onUpdateViaPass(final long viaIndex) {
        List<RouteParam> allPoiParamList = OpenApiHelper.getAllPoiParamList(
                MapType.MAIN_SCREEN_MAIN_MAP);
        // 删除途经点扎标
        mNaviPackage.removeViaPoint(MapType.CLUSTER_MAP, viaIndex + "");
        // 经过途经点后删除途经点
        if (allPoiParamList.size() > 2) {
            PoiInfoEntity poiInfo = allPoiParamList.get(1).getMPoiInfoEntity();
            boolean isDeleteSuccess = mRoutePackage.removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                    poiInfo, false);
            Logger.d(TAG, "onUpdateViaPass isDeleteSuccess = " + isDeleteSuccess);
        }
        mViewModel.onUpdateViaPass(viaIndex);
    }


    /**
     *  isShowLane 是否显示车道线
     *  laneInfoEntity 车道线信息
     */
    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        Logger.d(TAG, "onLaneInfo: " + (laneInfo) + "isShowLane "+  isShowLane);
        mViewModel.onLaneInfo(isShowLane, laneInfo);
    }

    /**
     * 算路失败回调
     * mapTypeId 屏幕id
     * errorMsg 失败信息
     */
    @Override
    public void onRouteFail(final MapType mapTypeId, final String errorMsg) {
        Logger.d(TAG, "onRouteFail");
    }

    @Override
    public void onRouteResult(final RequestRouteResult requestRouteResult) {
        Logger.d(TAG, "onRouteResult");
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.CLUSTER_MAP, ImersiveStatus.IMERSIVE);
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        NaviSceneManager.getInstance().destroySceneView();
        ImmersiveStatusScene.getInstance().unRegisterCallback(TAG);
        mLayerPackage.unRegisterCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        if (mNaviPackage != null) {
            mNaviPackage.unregisterObserver(TAG);
        }
        //移除导航状态监听
        NaviStatusMonitorUtil.getInstance().removeListener(mNaviStatusListener);
    }

    @Override
    public void onImmersiveStatusChange(final MapType mapTypeId,
                                        final ImersiveStatus currentImersiveStatus) {
        Logger.d(TAG, "NaviGuidanceModel currentImersiveStatus：" + currentImersiveStatus +
                "，mCurrentStatus：" + mCurrentStatus);
        if (!NaviStatus.NaviStatusType.NAVING.equals(NaviStatusPackage.getInstance().
                getCurrentNaviStatus())) {
            Logger.d(TAG, "Not in navigation, return");
            return;
        }
        if (currentImersiveStatus != mCurrentStatus ||
                currentImersiveStatus == ImersiveStatus.TOUCH) {
            mCurrentStatus = currentImersiveStatus;
            mViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }

    @Override
    public void deleteViaPoint(final NaviViaEntity entity) {
        ISceneCallback.super.deleteViaPoint(entity);
        if (!(entity.getChargeInfo() != null && entity.getChargeInfo().isAutoAdd())) {
            final PoiInfoEntity poiInfo = new PoiInfoEntity();
            poiInfo.setPoint(entity.getRealPos());
            poiInfo.setPid(entity.getPid());
            mRoutePackage.removeVia(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), poiInfo, true);
        }
    }

    /**
     * 更新场景可见性
     * @param sceneType 场景类型
     * @param isVisible 是否可见
     */
    @Override
    public void updateSceneVisible(final NaviSceneId sceneType, final boolean isVisible) {
        mViewModel.updateSceneVisible(sceneType, isVisible);
    }

    @Override
    public void onUpdateViaPass() {
        mViewModel.onUpdateViaPass(-1);
    }

    @Override
    public void onSuggestChangePath(long newPathID, long oldPathID,
                                    SuggestChangePathReasonEntity reason) {
        mNaviPackage.showMainAndSuggestPath(newPathID);
    }
    /***
     * 这里主要是 透出预计到达时间
     */
    @Override
    public void onUpdateElectVehicleETAInfo(List<FyElecVehicleETAInfo> infos) {
        IGuidanceObserver.super.onUpdateElectVehicleETAInfo(infos);
        Logger.d(TAG, "onUpdateElecVehicleETAInfo infos=="+infos);
    }

    @Override
    public void onDeleteChargeStationConfirmed() {
        deleteAutoAddChargeStation();
    }
    /**
     * 删除自动添加的充电站   更改状态条信息
     */
    public void deleteAutoAddChargeStation() {
        mIsShowAutoAdd = false;
        mViewModel.onUpdateViaList(mIsShowAutoAdd);
    }

    /**
     * 算路成功后 变为全览视角
     * @param successMsg 算路成功信息
     */
    @Override
    public void onRouteSuccess(String successMsg) {
        Logger.d(TAG, "onRouteSuccess");
        if (mNaviPackage.getPreviewStatus()) {
            OpenApiHelper.enterPreview(MapType.CLUSTER_MAP);
        }
    }

    /**
     * 车道线点击  点击后切换路线
     */
    @Override
    public void onRouteItemClick(MapType mapTypeId, LayerPointItemType type, LayerItemRoutePointClickResult result) {
        Logger.i(TAG, "onRouteItemClick result = " + result.toString());
        if (type == LayerPointItemType.ROUTE_PATH) {
            int currentNaviType = mNaviPackage.getCurrentNaviType();
            if (currentNaviType != 0) {
                Logger.i(TAG, "非GPS 导航，不支持手动切换路线");
                return;
            }
            if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                Logger.i(TAG, "离线状态，不支持手动切换路线");
                return;
            }
            long pathId = result.getIndex();
            int pathIndex = OpenApiHelper.getPathIndex(pathId);
            mNaviPackage.selectPath(MapType.CLUSTER_MAP, pathId, pathIndex);
        }
    }

    /**
     * 车牌号
     */
    public String getPlateNumber() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
    }

    /**
     *  限行开关
     */
    public String getAvoidLimit() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
    }

    /**
     * 路线偏好
     */
    public String getPreferences() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE);
    }

    /**
     * 补能计划
     */
    public String getEnergy() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN);
    }
}
