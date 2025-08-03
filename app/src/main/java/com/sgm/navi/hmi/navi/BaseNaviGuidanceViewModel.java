package com.sgm.navi.hmi.navi;

import android.app.Activity;
import android.app.Application;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.broadcast.FloatWindowReceiver;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.splitscreen.SplitFragment;
import com.sgm.navi.scene.api.route.ISceneRoutePreferenceCallBack;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.impl.navi.TimerHelper;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.scene.ui.navi.ChargeTipEntity;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.FyElecVehicleETAInfo;
import com.sgm.navi.service.define.navi.HandCardType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviModelSaveEntity;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.screen.ScreenTypeUtils;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.tts.NaviMediaPlayer;
import com.sgm.navi.ui.BuildConfig;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;


/**
 * @author sgm
 * @version $Revision.*$
 * description 导航Fragment视图模型
 */
public class BaseNaviGuidanceViewModel extends
        BaseViewModel<NaviGuidanceFragment, NaviGuidanceModel> implements
        ISceneRoutePreferenceCallBack, NaviGuidanceModel.OnNetStatusChangeListener,
        IBaseDialogClickListener, FloatWindowReceiver.FloatWindowCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_VIEW_MODEL;
    public ObservableField<Boolean> mNaviLanesVisibility;//车道线
    public ObservableField<Boolean> mNaviViaListVisibility;//途径点列表
    public ObservableField<Boolean> mNaviControlVisibilityMore;//control more
    public ObservableField<Boolean> mNaviViaInfoVisibility;//显示途径点信息
    public ObservableField<Boolean> mNaviLastMileVisibility;//最后一公里
    public ObservableField<Boolean> mNaviRouteNameVisibility;//当前路名
    public ObservableField<Boolean> mNaviParallelVisibility;//平行路
    public ObservableField<Boolean> mNaviPreferenceVisibility;//路线偏好
    public ObservableField<Boolean> mNaviTmcVisibility;//tmc
    public ObservableField<Boolean> mNaviEtaVisibility;//eta
    public ObservableField<Boolean> mNaviSpeedVisibility;//限速
    public ObservableField<Boolean> mNaviSapaVisibility;//Sapa
    public ObservableField<Boolean> mNaviCrossImageVisibility;//路口大图
    public ObservableField<Boolean> mNaviControlVisibility;//control
    public ObservableField<Boolean> mNaviViaArrivedPopVisibility;//途经点到达确认弹窗
    public ObservableField<Boolean> mNaviSapaDetailVisibility;//服务区/收费站详情页面
    public ObservableField<Boolean> mNaviDriveReportVisibility;//行程报告页面
    public ObservableField<Boolean> mNaviChargeTipVisibility;
    public ObservableField<Boolean> mNaviContinueVisibility;//继续导航
    private boolean mIsShowLane = false;
    public ObservableField<Boolean> mNaviRecChargeVisibility;//悬挂卡---推荐充电站
    public ObservableField<Boolean> mNaviRecGasVisibility;//悬挂卡---推荐加油站
    public ObservableField<Boolean> mNaviRecParkVisibility;//悬挂卡---推荐停车场
    public ObservableField<Boolean> mNaviRecChargeListVisibility;//沿途充电站列表
    public ObservableField<Boolean> mNaviRecGasListVisibility;//沿途加油站列表
    public ObservableField<Boolean> mNaviSim;
    public ObservableField<Boolean> mHandingCardVisibility;// 悬挂卡
    public ObservableField<Boolean> mHandingCardDetailVisibility;// 悬挂卡-详情
    public ObservableField<Boolean> mNaviViaDetailVisibility;// 途经点-详情
    public ObservableField<Boolean> mNaviLeftContentVisibility;// 引导左侧部分布局内容
    public ObservableField<Boolean> musicTabVisibility;
    //车牌信息
    private String mCurrentPlateNumber;
    //限行信息
    private String mCurrentavoidLimit;
    //路线偏好
    private String mCurrentPreferences;
    //补能规划
    private String mCurrentEnergy;
    private final NaviModelSaveEntity mModelSaveEntity;
    private HashMap<NaviSceneId, Integer> mSceneStatus;
    private boolean mIsOverView;
    private boolean mIsFixedOverView;
    private boolean mIsRoadNameCanShow;

    public BaseNaviGuidanceViewModel(@NonNull final Application application) {
        super(application);
        mNaviViaInfoVisibility = new ObservableField<>(false);
        mNaviLastMileVisibility = new ObservableField<>(false);
        mNaviRouteNameVisibility = new ObservableField<>(false);
        mNaviParallelVisibility = new ObservableField<>(false);
        mNaviLanesVisibility = new ObservableField<>(false);
        mNaviViaListVisibility = new ObservableField<>(false);
        mNaviControlVisibilityMore = new ObservableField<>(false);
        mNaviPreferenceVisibility = new ObservableField<>(false);
        mNaviTmcVisibility = new ObservableField<>(true);
        mNaviEtaVisibility = new ObservableField<>(true);
        mNaviSpeedVisibility = new ObservableField<>(false);
        mNaviSapaVisibility = new ObservableField<>(false);
        mNaviCrossImageVisibility = new ObservableField<>(false);
        mNaviControlVisibility = new ObservableField<>(false);
        mNaviViaArrivedPopVisibility = new ObservableField<>(false);
        mNaviSapaDetailVisibility = new ObservableField<>(false);
        mNaviDriveReportVisibility = new ObservableField<>(false);
        mNaviChargeTipVisibility = new ObservableField<>(false);
        mNaviContinueVisibility = new ObservableField<>(false);
        mNaviRecChargeVisibility = new ObservableField<>(false);
        mNaviRecGasVisibility = new ObservableField<>(false);
        mNaviRecChargeListVisibility = new ObservableField<>(false);
        mNaviRecGasListVisibility = new ObservableField<>(false);
        mNaviRecParkVisibility = new ObservableField<>(false);
        mNaviSim = new ObservableField<>(false);
        mHandingCardVisibility = new ObservableField<>(false);
        mHandingCardDetailVisibility = new ObservableField<>(false);
        mNaviViaDetailVisibility = new ObservableField<>(false);
        mNaviLeftContentVisibility = new ObservableField<>(true);
        musicTabVisibility = new ObservableField<>(false);
        mModelSaveEntity = new NaviModelSaveEntity();
        mSceneStatus = new HashMap<>();
    }

    @Override
    protected NaviGuidanceModel initModel() {
        return new NaviGuidanceModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mModel.addOnNetStatusChangeListener(this);
        // 注册媒体悬浮窗广播
        FloatWindowReceiver.registerCallback(TAG, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mModel.removeOnNetStatusChangeListener(this);
        NaviMediaPlayer.getInstance().releaseMediaPlayer();
        FloatWindowReceiver.unregisterCallback(TAG);
    }

    //显示/隐藏 添加途径点页面
    public final Action mNaviAddVia = this::onSwitchViaList;

    public void initShowScene(NaviSceneId... sceneIds) {
        for (NaviSceneId sceneId : sceneIds) {
            NaviSceneManager.getInstance().initShowScene(sceneId);
        }
    }

    @Override
    public void onWindowSideChanged(boolean isOpenFloat) {
        Logger.d(TAG, "悬浮窗开关：" + isOpenFloat);
        musicTabVisibility.set(isOpenFloat && ScreenTypeUtils.getInstance().isFullScreen());
        mModel.setMapCenterInScreen(isOpenFloat);
    }

    /**
     * 显示/隐藏 途径点列表
     */
    public void onSwitchViaList() {
        if (Objects.equals(mNaviCrossImageVisibility.get(), Boolean.TRUE)) {
            return;
        }
        if (!TimerHelper.isCanDo()) {
            return;
        }
        final Boolean b = mNaviViaListVisibility.get();
        final boolean visible = Boolean.FALSE.equals(b);
        final List<NaviViaEntity> viaList = mModel.getViaList();
        if (!ConvertUtils.isEmpty(viaList)) {
            mView.updateViaListState(viaList, false);
            mModel.updateViaListState(viaList);
        }
        mView.showNaviViaList(visible);
    }

    /**
     * 更新场景可见性
     *
     * @param sceneType scene type
     * @param isVisible is visible
     */
    public void updateSceneVisible(final NaviSceneId sceneType, final boolean isVisible) {
        Logger.i(TAG, "sceneType:", sceneType, ",isVisible:", isVisible);
        if (mView == null) {
            Logger.w(TAG, "updateSceneVisible mView is null");
            return;
        }
        switch (sceneType) {
            case NAVI_SCENE_3D_CROSS:
            case NAVI_SCENE_2D_CROSS:
                mView.onCrossImageInfo(isVisible);
                mNaviCrossImageVisibility.set(isVisible);
                mView.updateViewRadius();
                mView.updateSclTopClickState(!isVisible);
                break;
            case NAVI_SCENE_ETA:
                mNaviEtaVisibility.set(isVisible);
                mView.updateViewRadius();
                break;
            case NAVI_SCENE_LANES:
                mNaviLanesVisibility.set(isVisible);
                mView.updateViewRadius();
                break;
            case NAVI_SCENE_VIA_POINT_LIST:
                mNaviViaListVisibility.set(isVisible);
                mView.setViaListVisibility(isVisible);
                break;
            case NAVI_SCENE_SERVICE_AREA:
                mNaviSapaVisibility.set(isVisible);
                mView.setSapaVisibility(isVisible);
                break;
            case NAVI_SCENE_PARALLEL:
                mNaviParallelVisibility.set(isVisible);
                break;
            case NAVI_SCENE_CONTROL_MORE:
                mNaviControlVisibilityMore.set(isVisible);
                mView.setControlMoreVisibility(isVisible);
                break;
            case NAVI_SCENE_CONTROL:
                mNaviControlVisibility.set(isVisible);
                break;
            case NAVI_SCENE_LAST_MILE:
                mNaviLastMileVisibility.set(isVisible);
                break;
            case NAVI_SCENE_PREFERENCE:
                mNaviPreferenceVisibility.set(isVisible);
                break;
            case NAVI_SCENE_SPEED:
                mNaviSpeedVisibility.set(isVisible);
                break;
            case NAVI_SCENE_TMC:
                mNaviTmcVisibility.set(isVisible && mModel.isNetConnected());
                mView.updateViewRadius();
                break;
            case NAVI_SCENE_VIA_DETAIL_INFO:
                mNaviViaInfoVisibility.set(isVisible);
                mView.updateViewRadius();
                break;
            case NAVI_VIA_ARRIVED_POP:
                mNaviViaArrivedPopVisibility.set(isVisible);
                break;
            case NAVI_SAPA_DETAIL_INFO:
                mNaviSapaDetailVisibility.set(isVisible);
                mView.setSapaDetailVisibility(isVisible);
                break;
            case NAVI_DRIVE_REPORT:
                mNaviDriveReportVisibility.set(isVisible);
                break;
            case NAVI_CHARGE_TIP:
                mNaviChargeTipVisibility.set(isVisible);
                break;
            case NAVI_CONTINUE:
                mNaviContinueVisibility.set(isVisible);
                //For Bury Point
                if (isVisible) {
                    sendBuryPointForWakeup();
                }
                break;
            case NAVI_SUSPEND_CARD:
                mHandingCardVisibility.set(isVisible);
                break;
            case NAVI_SUSPEND_CARD_DETAIL:
                mHandingCardDetailVisibility.set(isVisible);
                break;
            case NAVI_SCENE_VIA_DETAIL:
                mNaviViaDetailVisibility.set(isVisible);
                mView.setViaDetailVisibility(isVisible);
                break;
            default:
                break;
        }
    }

    /**
     * 开始导航
     *
     * @param bundle bundle
     */
    public void startNavigation(final Bundle bundle) {
        mModel.startNavigation(bundle);
    }

    /**
     * 恢复导航后更新地图模式
     */
    public void refreshMapMode() {
        MapMode currentMapMode = MapPackage.getInstance().getCurrentMapMode(MapType.MAIN_SCREEN_MAIN_MAP);
        MapPackage.getInstance().switchMapMode(MapType.MAIN_SCREEN_MAIN_MAP, currentMapMode, true);
        LayerPackage.getInstance().setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, true);
    }

    /**
     * 区间车速、绿波车速
     *
     * @param speedCameraInfo speed camera info
     */
    public void onNaviSpeedCameraInfo(final SpeedOverallEntity speedCameraInfo) {
        if (ConvertUtils.isEmpty(speedCameraInfo)) {
            Logger.i(TAG, "onNaviSpeedOverallInfo speedCameraInfo is null");
            return;
        }
        mModelSaveEntity.setSpeedOverallEntity(speedCameraInfo);
        mView.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    /**
     * 服务区信息
     *
     * @param sapaInfoEntity sapa info entity
     */
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        if (ConvertUtils.isEmpty(sapaInfoEntity)) {
            Logger.i(TAG, "onNaviSAPAInfo sapaInfoEntity is null");
            return;
        }
        mModelSaveEntity.setSapaInfoEntity(sapaInfoEntity);
        mView.onNaviSAPAInfo(sapaInfoEntity);
    }

    /**
     * 导航信息
     *
     * @param naviEtaInfo navi eta info
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        mModelSaveEntity.setNaviEtaInfo(naviEtaInfo);
        updateRouteName(naviEtaInfo);
        mView.onNaviInfo(naviEtaInfo);
    }

    public void onNaviInfoByViaArrived(final NaviEtaInfo naviEtaInfo) {
        if (mView == null) {
            return;
        }
        mView.onNaviInfoByViaArrived(naviEtaInfo);
    }

    /**
     * 更新路线名称
     *
     * @param naviEtaInfo navi eta info
     */
    private void updateRouteName(final NaviEtaInfo naviEtaInfo) {
        if (!TextUtils.isEmpty(naviEtaInfo.getCurRouteName())) {
            if (!mIsRoadNameCanShow) {
                mIsRoadNameCanShow = true;
                showRoadName();
            }
            mView.updateRouteName(naviEtaInfo.getCurRouteName());
        }
    }

    /**
     * 导航停止
     */
    public void onNaviStop() {
        if (!ConvertUtils.isNull(mView)) {
            mView.onNaviStop();
        }
        closeAllFragment();
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            addFragment(new SplitFragment(), new Bundle());
        }
    }

    /**
     * 导航信息
     *
     * @param routeRemainDist 路口大图进度
     */
    public void onCrossProgress(final long routeRemainDist) {
        mModelSaveEntity.setCrossProgress(routeRemainDist);
        mView.updateCrossProgress(routeRemainDist);
    }

    /**
     * 路口大图
     *
     * @param isShowImage   是否显示图片
     * @param naviImageInfo 导航图片信息
     */
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        mModelSaveEntity.setCrossImageEntity(naviImageInfo);
        mView.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    /**
     * 更新TMC灯光条（路况信息）
     *
     * @param naviTmcInfo navi tmc info
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, final boolean isShow) {
        if (ConvertUtils.isEmpty(naviTmcInfo)) {
            Logger.i(TAG, "onUpdateTMCLightBar naviTmcInfo is null");
            return;
        }
        mModelSaveEntity.setNaviTmcInfo(naviTmcInfo);
        mView.onUpdateTMCLightBar(naviTmcInfo, isShow);
    }

    /**
     * 转向图标信息、以及传出出入口信息
     *
     * @param info maneuver info
     */
    public void onManeuverInfo(final NaviManeuverInfo info) {
        if (ConvertUtils.isEmpty(info)) {
            Logger.i(TAG, "onManeuverInfo info is null");
            return;
        }
        mModelSaveEntity.setNaviManeuverInfo(info);
        mView.onManeuverInfo(info);
    }

    /**
     * 导航到达目的地
     *
     * @param traceId  trace id
     * @param naviType navi type
     */
    public void onNaviArrive(final long traceId, final int naviType) {
        mView.onNaviArrive(traceId, naviType);
    }

    /**
     * 车道线信息
     *
     * @param isShowLane 是否显示车道线
     * @param laneInfo   lane info
     */
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        mModelSaveEntity.setLaneInfo(laneInfo);
        mIsShowLane = isShowLane;
        mView.onLaneInfo(isShowLane, laneInfo);
    }

    /**
     * 沉浸态状态改变回调
     *
     * @param currentImersiveStatus current immersive status
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "onImmersiveStatusChange固定全览：",
                NaviPackage.getInstance().getFixedOverViewStatus(),
                " currentImersiveStatus:", currentImersiveStatus);
        mView.onImmersiveStatusChange(currentImersiveStatus);
        showRoadName();
    }

    private void showRoadName() {
        ImersiveStatus currentImersiveStatus = ImmersiveStatusScene.getInstance().
                getCurrentImersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP);
        // 1036921 继续导航显示的时候不显示当前道路名称
        mNaviRouteNameVisibility.set((currentImersiveStatus == ImersiveStatus.IMERSIVE) &&
                Boolean.FALSE.equals(mNaviContinueVisibility.get()) && mIsRoadNameCanShow);
    }

    /**
     * 添加场景回调
     *
     * @param sceneCallback scene callback
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mView.addSceneCallback(sceneCallback);
    }

    /**
     * 显示导航偏好页面
     */
    public void showNaviPreferenceScene() {
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneShowState,
                NaviSceneId.NAVI_SCENE_PREFERENCE);
    }

    /**
     * 显示控制条更多页面
     */
    public void showNaviControlMoreScene() {
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneShowState,
                NaviSceneId.NAVI_SCENE_CONTROL_MORE);
    }

    /**
     * 显示控制条页面
     */
    public void showNaviControlScene() {
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneShowState,
                NaviSceneId.NAVI_SCENE_CONTROL);
    }

    /**
     * 路由偏好改变回调
     *
     * @param text          text
     * @param isFirstChange 是否是第一次改变
     */
    @Override
    public void onRoutePreferenceChange(final String text, final boolean isFirstChange) {
        Logger.d(TAG, "text：", text, ",isFirstChange：", isFirstChange);
        if (!isFirstChange) {
            mModel.onRoutePreferenceChange();
        }
    }

    /**
     * 途经点通过回调
     *
     * @param viaIndex via index
     */
    public void onUpdateViaPass(final long viaIndex) {
        mView.onUpdateViaPass(viaIndex);
    }

    /**
     * 删除途经点结果回调
     *
     * @param result result
     * @param entity entity
     */
    public void notifyDeleteViaPointResult(final boolean result, final NaviViaEntity entity) {
        mView.notifyDeleteViaPointResult(result, entity);
    }

    /***
     * 电量低提醒
     * @param entity
     */
    public void notifyBatteryWarning(ChargeTipEntity entity) {
        mView.notifyBatteryWarning(entity);
    }

    /**
     * 跳转服务区详情页方法
     *
     * @param type           type
     * @param sapaInfoEntity sapa info entity
     */
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        if (mModelSaveEntity != null) {
            mModelSaveEntity.setSapaDetailType(type);
        }
        mView.skipNaviSapaDetailScene(type, sapaInfoEntity);
    }

    /**
     * 导航继续
     */
    public void naviContinue() {
        mView.naviContinue();
    }

    /**
     * @param type 平行路切换类型 0:主辅路切换 1:桥上下切换
     */
    public void naviParallelSwitch(final int type) {
        mView.naviParallelSwitch(type);
    }

    public Activity getActivity(){
        return mView.getActivity();
    }

    /**
     * @param type 0:退出全览 1:切换全览
     */
    public void naviPreviewSwitch(final int type) {
        mView.naviPreviewSwitch(type);
    }

    @Override
    public void onNetStatusChange(boolean isConnected) {
        // 离线隐藏光柱图，在线显示
        Logger.d(TAG, "离线隐藏光柱图，在线显示 isConnected:", isConnected);
        mNaviTmcVisibility.set(isConnected);
        mView.onNetStatusChange(isConnected);
    }

    /**
     * 显示控制详情
     */
    public void showControlDetails() {
        mView.showControlDetails();
    }

    public void goSearchView(final String keyWord, final int searchType) {
        mView.goSearchView(keyWord, searchType);
    }

    public void goAlongWayList() {
        mView.goAlongWayList();
    }

    public void closeSearchView() {
        mView.closeSearchView();
    }

    /**
     * 更新via列表
     */
    public void updateViaList() {
        final List<NaviViaEntity> viaList = mModel.getViaList();
        if (!ConvertUtils.isEmpty(viaList)) {
            if (mNaviViaListVisibility != null &&
                    Boolean.TRUE.equals(mNaviViaListVisibility.get())) {
                mView.updateViaListState(viaList, false);
            }
            mView.refreshViaInfo();
            mModel.updateViaListState(viaList);
        }
    }

    /**
     * 立即更新via列表
     */
    public void updateViaListImm() {
        final List<NaviViaEntity> viaList = mModel.getViaList();
        if (!ConvertUtils.isEmpty(viaList)) {
            mView.updateViaListState(viaList, false);
            mModel.updateViaListState(viaList);
        }
    }

    public void showDeleteAllTip() {
        new ChargeStationDeletTipDialog(mView.getActivity(), new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                IBaseDialogClickListener.super.onCommitClick();
                Logger.i(TAG, "确定删除！");
                mModel.deleteAutoAddChargeStation();
                mModel.clearAllViaChargeStation();
            }
        }).show();
    }

    public void onUpdateViaList(boolean isShow) {
        mView.onUpdateTMCLightBarAutoAdd(isShow);
        final List<NaviViaEntity> viaList = mModel.getViaList();
        if (!ConvertUtils.isEmpty(viaList)) {
            mView.updateViaListState(viaList, true);
            mModel.updateViaListState(viaList);
        } else {
            if(Logger.openLog) {
                Logger.d(TAG, " viaList is empty");
            }
        }
    }

    public void onUpdateElectVehicleETAInfo(final List<FyElecVehicleETAInfo> infos) {
        if (null != mView) {
            mView.onUpdateElectVehicleETAInfo(infos);
        }
    }

    public void backToNaviFragment() {
        mView.backToNaviFragment();
    }

    /***
     * 设置改变请求重新请求算路
     */
    public void isRequestRouteForPlateNumberAndAvoidLimitChange() {
        Logger.d(TAG, "isRequestRouteForPlateNumberAndAvoidLimitChange");
        if (Objects.equals(mCurrentPlateNumber, mModel.getPlateNumber()) &&
                Objects.equals(mCurrentavoidLimit, mModel.getAvoidLimit()) &&
                Objects.equals(mCurrentEnergy, mModel.getEnergy()) &&
                Objects.equals(mCurrentPreferences, mModel.getPreferences())) {
            Logger.d(TAG, "isRequestRouteForPlateNumberAndAvoidLimitChange 没有改变");
            return;
        }
        ThreadManager.getInstance().execute(() -> {
            final RouteRequestParam param = new RouteRequestParam();
            param.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
            RoutePackage.getInstance().requestRoute(param);
        });
        ThreadManager.getInstance().postUi(() -> {
            if (mView != null) {
                mView.updatePreference();
            }
        });
        setDefultPlateNumberAndAvoidLimitSave();
    }

    /***
     * 保存车牌和限行的数据
     */
    public void setDefultPlateNumberAndAvoidLimitSave() {
        Logger.d(TAG, "setDefultPlateNumberAndAvoidLimitSave");
        mCurrentPlateNumber = mModel.getPlateNumber();
        mCurrentavoidLimit = mModel.getAvoidLimit();
        mCurrentPreferences = mModel.getPreferences();
        mCurrentEnergy = mModel.getEnergy();
    }

    public void setCurrentPreferences(String currentPreferences) {
        mCurrentPreferences = currentPreferences;
    }

    public void setNaviSimState(boolean isSim) {
        mNaviSim.set(isSim && BuildConfig.DEBUG);
    }

    public void hideNaviContent() {
        if (null != mView) {
            mView.hideNaviContent();
        }
    }

    public boolean isNeedPreViewShowList() {
        return mHandingCardDetailVisibility.get() || mNaviViaListVisibility.get();
    }

    public HashMap<NaviSceneId, Integer> getSceneStatus() {
        return mSceneStatus;
    }

    public void setSceneStatus(HashMap<NaviSceneId, Integer> map) {
        mSceneStatus = map;
    }

    /**
     * 保存全览模式状态
     */
    public void saveOverViewStatus() {
        mIsFixedOverView = NaviPackage.getInstance().getFixedOverViewStatus();
        mIsOverView = NaviPackage.getInstance().getPreviewStatus();
        Logger.i(TAG, "saveOverViewStatus mIsFixedOverView:", mIsFixedOverView,
                " mIsOverView:", mIsOverView);
    }

    public void restoreOverViewStatus() {
        NaviPackage.getInstance().setFixedOverViewStatus(mIsFixedOverView);
        NaviPackage.getInstance().setPreviewStatus(mIsOverView);
        Logger.i(TAG, "restoreOverViewStatus mIsFixedOverView:", mIsFixedOverView,
                " mIsOverView:", mIsOverView);
    }

    /**
     * 恢复导航数据
     */
    public void restoreNavigation() {
        if (ConvertUtils.isEmpty(mModelSaveEntity)) {
            Logger.i(TAG, "mModelSaveEntity is null");
            return;
        }
        Logger.i(TAG, "restoreNavigation");
        final SpeedOverallEntity speedOverallEntity = mModelSaveEntity.getSpeedOverallEntity();
        boolean isNeedShowSpeedCameraInfo = !ConvertUtils.isNull(mNaviSpeedVisibility) &&
                Boolean.TRUE.equals(mNaviSpeedVisibility.get()) && speedOverallEntity != null;
        if (isNeedShowSpeedCameraInfo) {
            onNaviSpeedCameraInfo(speedOverallEntity);
        }
        onNaviSAPAInfo(mModelSaveEntity.getSapaInfoEntity());
        final NaviEtaInfo naviEtaInfo = mModelSaveEntity.getNaviEtaInfo();
        boolean isNeedRestoreEtaInfo = naviEtaInfo != null;
        if (isNeedRestoreEtaInfo) {
            onNaviInfo(naviEtaInfo);
        }
        final CrossImageEntity naviImageInfo = mModelSaveEntity.getCrossImageEntity();
        boolean isNeedRestoreCrossImageInfo = naviImageInfo != null &&
                !ConvertUtils.isNull(mNaviCrossImageVisibility) &&
                Boolean.TRUE.equals(mNaviCrossImageVisibility.get());
        if (isNeedRestoreCrossImageInfo) {
            onCrossProgress(mModelSaveEntity.getCrossProgress());
            onCrossImageInfo(true, naviImageInfo);
        }
        NaviTmcInfo naviTmcInfo = mModelSaveEntity.getNaviTmcInfo();
        boolean isNeedRestoreTmcInfo = naviTmcInfo != null;
        if (isNeedRestoreTmcInfo) {
            onUpdateTMCLightBar(naviTmcInfo, mModel == null || mModel.getIsShowAutoAdd());
        }
        NaviManeuverInfo info = mModelSaveEntity.getNaviManeuverInfo();
        boolean isNeedRestoreManeuverInfo = info != null;
        if (isNeedRestoreManeuverInfo) {
            onManeuverInfo(info);
        }
        LaneInfoEntity laneInfo = mModelSaveEntity.getLaneInfo();
        boolean isNeedRestoreLaneInfo = laneInfo != null &&
                !ConvertUtils.isNull(mNaviLanesVisibility) &&
                Boolean.TRUE.equals(mNaviLanesVisibility.get());
        if (isNeedRestoreLaneInfo) {
            onLaneInfo(mIsShowLane, laneInfo);
        }
        List<NaviViaEntity> viaList = mModelSaveEntity.getViaList();
        if (mView != null) {
            mView.updateViaListState(viaList, false);
        }
        showViaDetail(mModelSaveEntity.isIsViaDetailShow());
        updateNewestViaPoint(mModelSaveEntity.getNaviViaEntity());
        if (!ConvertUtils.isNull(mNaviSapaDetailVisibility) &&
                Boolean.TRUE.equals(mNaviSapaDetailVisibility.get())) {
            skipNaviSapaDetailScene(mModelSaveEntity.getSapaDetailType(),
                    mModelSaveEntity.getSapaInfoEntity());
        }
        if (!ConvertUtils.isNull(mView)) {
            mView.updateViewRadius();
        }
        if (Boolean.TRUE.equals(mHandingCardDetailVisibility.get())) {
            if (!ConvertUtils.isNull(mView)) {
                mView.restoreHandingCardView(
                        mModelSaveEntity.getHangingCardPoiList(),
                        mModelSaveEntity.getHandCardType());
            }
        }
    }

    public void onCurrentRoadSpeed(int speed) {
        if (null != mView) {
            mView.onCurrentRoadSpeed(speed);
        }
    }

    /**
     * @param b 是否达到了显示途经点详情的时机
     */
    public void showViaDetail(boolean b) {
        if (mModelSaveEntity != null) {
            mModelSaveEntity.setIsViaDetailShow(b);
        }
        if (null != mView) {
            mView.showViaDetail(b);
        }
    }

    /**
     * @param naviViaEntity 最新的途经点信息
     */
    public void updateNewestViaPoint(NaviViaEntity naviViaEntity) {
        if (mModelSaveEntity != null) {
            mModelSaveEntity.setNaviViaEntity(naviViaEntity);
        }
        if (null != mView) {
            mView.updateNewestViaPoint(naviViaEntity);
        }
    }
    public void onMeterAction() {
        if (null != mView) {
            mView.onMeterAction();
        }
    }

    public void onPassByClick() {
        Logger.i(TAG, "onPassByClick");
        if (mView != null) {
            mView.onPassByClick();
        }
    }

    public void restoreNavigationByRebuild() {
        if (mModel != null) {
            mModel.restoreNavigationByRebuild();
        }
    }

    public void initLazyView() {
        if (mView != null) {
            mView.initLazyView();
        }
    }

    public void saveViaList(List<NaviViaEntity> mViaList) {
        if (mModelSaveEntity != null) {
            mModelSaveEntity.setViaList(mViaList);
        }
    }

    public void onMapClick() {
        if (mView != null) {
            mView.onMapClick();
        }
    }

    public boolean getIsViaArrived() {
        if (mModel != null) {
            return mModel.getIsViaArrived();
        }
        return false;
    }

    public void onMapSwipe() {
        if (mView != null) {
            mView.onMapSwipe();
        }
    }

    //埋点使用，不准删除
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_WAKEUP)
    private void sendBuryPointForWakeup() {
    }


    @Override
    public void onCancelClick() {
        mModel.cancelRoute();
    }

    /***
     * 展示算路弹框
     */
    public void showProgressUI() {
        ThreadManager.getInstance().postUi(() -> {
            mView.showProgressUI();
            //重算路关闭Route/Guidance上面所有页面
            closeAllFragmentUpRoute();
        });
    }

    /***
     * 隐藏算路弹框
     * @param success 算路成功
     */
    public void hideProgressUI(final boolean success) {
        ThreadManager.getInstance().postUi(() -> {
            mView.hideProgressUI();
            if (success) {
                mModel.showSuccessMsg();
            }
        });
    }

    /***
     * 展示离线算路中弹框
     */
    public void showOfflineProgressUI() {
        ThreadManager.getInstance().postUi(() -> {
            mView.showOfflineProgressUI();
        });
    }

    /***
     * 只展示算路中弹框
     */
    public void showProgressUIOnly() {
        ThreadManager.getInstance().postUi(() -> {
            mView.showProgressUI();
        });
    }

    public void deleteAutoAddChargeStation() {
        if (mModel != null) {
            mModel.deleteAutoAddChargeStation();
        }
    }

    public void saveHandingCardDetail(List<PoiInfoEntity> infoEntities, HandCardType type) {
        if (mModelSaveEntity != null) {
            mModelSaveEntity.setHandCardType(type);
            mModelSaveEntity.setHangingCardPoiList(infoEntities);
        }
    }
}
