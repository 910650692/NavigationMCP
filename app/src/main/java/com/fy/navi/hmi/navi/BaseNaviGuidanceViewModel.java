package com.fy.navi.hmi.navi;

import android.app.Application;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.api.route.ISceneRoutePreferenceCallBack;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.ChargeTipEntity;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.util.List;


/**
 * @author fy
 * @version $Revision.*$
 * description 导航Fragment视图模型
 */
public class BaseNaviGuidanceViewModel extends
        BaseViewModel<NaviGuidanceFragment, NaviGuidanceModel> implements
        ISceneRoutePreferenceCallBack, NaviGuidanceModel.OnNetStatusChangeListener {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    public ObservableField<Boolean> mNaviLanesVisibility;//车道线
    public ObservableField<Boolean> mNaviViaListVisibility;//途径点列表
    public ObservableField<Boolean> mNaviParkingListVisibility;//停车场列表
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

    public BaseNaviGuidanceViewModel(@NonNull final Application application) {
        super(application);
        mNaviViaInfoVisibility = new ObservableField<>(false);
        mNaviLastMileVisibility = new ObservableField<>(false);
        mNaviRouteNameVisibility = new ObservableField<>(true);
        mNaviParallelVisibility = new ObservableField<>(false);
        mNaviLanesVisibility = new ObservableField<>(false);
        mNaviViaListVisibility = new ObservableField<>(false);
        mNaviParkingListVisibility = new ObservableField<>(false);
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
    }

    @Override
    protected NaviGuidanceModel initModel() {
        return new NaviGuidanceModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mModel.addOnNetStatusChangeListener(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mModel.removeOnNetStatusChangeListener(this);
    }

    //显示/隐藏 添加途径点页面
    public final Action mNaviAddVia = this::onSwitchViaList;

    /**
     * 显示/隐藏 途径点列表
     */
    public void onSwitchViaList() {
        final Boolean b = mNaviViaListVisibility.get();
        final boolean visible = Boolean.FALSE.equals(b);
        final List<NaviViaEntity> viaList = mModel.getViaList();
        mView.showNaviViaList(visible);
        mView.updateViaListState(viaList);
    }

    /**
     * 更新场景可见性
     *
     * @param sceneType scene type
     * @param isVisible is visible
     */
    public void updateSceneVisible(final NaviSceneId sceneType, final boolean isVisible) {
        Logger.i(TAG, "sceneType:" + sceneType + ",isVisible:" + isVisible);
        switch (sceneType) {
            case NAVI_SCENE_3D_CROSS:
            case NAVI_SCENE_2D_CROSS:
                Logger.i(TAG, "路口大图展示状态：" + isVisible);
                mNaviCrossImageVisibility.set(isVisible);
                break;
            case NAVI_SCENE_ETA:
                mNaviEtaVisibility.set(isVisible);
                break;
            case NAVI_SCENE_LANES:
                mNaviLanesVisibility.set(isVisible);
                break;
            case NAVI_SCENE_TBT:
                Logger.i(TAG, "TBT面板不会被隐藏");
                break;
            case NAVI_SCENE_VIA_POINT_LIST:
                Logger.i(TAG, "途径点面板展示状态：" + isVisible);
                mNaviViaListVisibility.set(isVisible);
                break;
            case NAVI_SCENE_SERVICE_AREA:
                mNaviSapaVisibility.set(isVisible);
                break;
            case NAVI_SCENE_PARALLEL:
                mNaviParallelVisibility.set(isVisible);
                break;
            case NAVI_SCENE_PARK_LIST:
                mNaviParkingListVisibility.set(isVisible);
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
                mNaviTmcVisibility.set(isVisible);
                break;
            case NAVI_SCENE_VIA_DETAIL_INFO:
                mNaviViaInfoVisibility.set(isVisible);
                break;
            case NAVI_VIA_ARRIVED_POP:
                mNaviViaArrivedPopVisibility.set(isVisible);
                break;
            case NAVI_SAPA_DETAIL_INFO:
                mNaviSapaDetailVisibility.set(isVisible);
                break;
            case NAVI_DRIVE_REPORT:
                mNaviDriveReportVisibility.set(isVisible);
                break;
            case NAVI_CHARGE_TIP:
                mNaviChargeTipVisibility.set(isVisible);
                break;
            case NAVI_CONTINUE:
                mNaviContinueVisibility.set(isVisible);
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
        mView.startNavigation();
    }

    /**
     * 区间车速、绿波车速
     *
     * @param speedCameraInfo speed camera info
     */
    public void onNaviSpeedCameraInfo(final SpeedOverallEntity speedCameraInfo) {
        mView.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    /**
     * 服务区信息
     *
     * @param sapaInfoEntity sapa info entity
     */
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        mView.onNaviSAPAInfo(sapaInfoEntity);
    }

    /**
     * 导航信息
     *
     * @param naviEtaInfo navi eta info
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        updateRouteName(naviEtaInfo);
        mView.onNaviInfo(naviEtaInfo);
    }

    /**
     * 更新路线名称
     *
     * @param naviEtaInfo navi eta info
     */
    private void updateRouteName(final NaviEtaInfo naviEtaInfo) {
        if (!TextUtils.isEmpty(naviEtaInfo.getCurRouteName())) {
            mView.updateRouteName(naviEtaInfo.getCurRouteName());
        }
    }

    /**
     * 导航停止
     */
    public void onNaviStop() {
        mView.onNaviStop();
        closeAllFragment();
    }

    /**
     * 导航信息
     *
     * @param routeRemainDist 路口大图进度
     */
    public void onCrossProgress(final long routeRemainDist) {
        mView.updateCrossProgress(routeRemainDist);
    }

    /**
     * 路口大图
     *
     * @param isShowImage   是否显示图片
     * @param naviImageInfo 导航图片信息
     */
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        mView.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    /**
     * 更新TMC灯光条（路况信息）
     *
     * @param naviTmcInfo navi tmc info
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, final boolean isShow) {
        mView.onUpdateTMCLightBar(naviTmcInfo, isShow);
    }

    /**
     * 转向图标信息、以及传出出入口信息
     *
     * @param info maneuver info
     */
    public void onManeuverInfo(final NaviManeuverInfo info) {
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
        mIsShowLane = isShowLane;
        mView.onLaneInfo(isShowLane, laneInfo);
    }

    /**
     * 沉浸态状态改变回调
     *
     * @param currentImersiveStatus current immersive status
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        mNaviRouteNameVisibility.set(currentImersiveStatus == ImersiveStatus.IMERSIVE);
        mView.onImmersiveStatusChange(currentImersiveStatus);
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
     * 路由偏好改变回调
     *
     * @param text          text
     * @param isFirstChange 是否是第一次改变
     */
    @Override
    public void onRoutePreferenceChange(final String text, final boolean isFirstChange) {
        Logger.d(TAG, "text：" + text + ",isFirstChange：" + isFirstChange);
        if (!isFirstChange) {
            mNaviPreferenceVisibility.set(false);
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
        mView.skipNaviSapaDetailScene(type, sapaInfoEntity);
    }

    /**
     * 行程报告回调
     *
     * @param entity entity
     */
    public void onDriveReport(final NaviDriveReportEntity entity) {
        mView.onDriveReport(entity);
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

    /**
     * @param type 0:退出全览 1:切换全览
     */
    public void naviPreviewSwitch(final int type) {
        mView.naviPreviewSwitch(type);
    }

    @Override
    public void onNetStatusChange(boolean isConnected) {
        // 离线隐藏光柱图，在线显示
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
        // 延时500ms是为了等数据添加完成
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                final List<NaviViaEntity> viaList = mModel.getViaList();
                mView.updateViaListState(viaList);
            }
        }, NumberUtils.NUM_500);
    }

    public void showDeleteAllTip() {
        new ChargeStationDeletTipDialog(mView.getActivity(), new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                IBaseDialogClickListener.super.onCommitClick();
                Logger.i(TAG, "确定删除！");
                mModel.deleteAutoAddChargeStation();
            }
        }).show();
    }

    public void onUpdateViaList(boolean isShow) {
        mView.updateViaListState(mModel.getViaList());
        mView.onUpdateTMCLightBarAutoAdd(isShow);
    }

    public boolean isNeedCloseNaviChargeTipLater() {
        return mView.isNeedCloseNaviChargeTipLater();
    }
}
