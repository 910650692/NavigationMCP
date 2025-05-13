package com.fy.navi.hmi.cluster.cluster_navi_info;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.cluster.utils.NetworkMonitorUtil;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.navi.CrossImageEntity;

import com.fy.navi.service.define.navi.LaneInfoEntity;

import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;


/**
 * @author fy
 * @version $Revision.*$
 * description 导航Fragment视图模型
 */
public class BaseClusterNaviGuidanceViewModel extends
        BaseViewModel<NaviClusterGuidanceFragment, ClusterNaviGuidanceModel>{
    private static final String TAG = "BaseNaviGuidanceTwoTwoViewModel";
    public ObservableField<Boolean> mNaviLanesVisibility;//车道线
    public ObservableField<Boolean> mNaviViaInfoVisibility;//显示途径点信息

    public ObservableField<Boolean> mNaviRouteNameVisibility;//当前路名
    public ObservableField<Boolean> mNaviPreferenceVisibility;//路线偏好
    public ObservableField<Boolean> mNaviTmcVisibility;//tmc
    public ObservableField<Boolean> mNaviEtaVisibility;//eta
    public ObservableField<Boolean> mNaviSpeedVisibility;//限速
    public ObservableField<Boolean> mNaviSapaVisibility;//Sapa
    public ObservableField<Boolean> mNaviCrossImageVisibility;//路口大图
    private final ArrayList<Integer> mSceneStatus;

    public BaseClusterNaviGuidanceViewModel(@NonNull final Application application) {
        super(application);
        mNaviViaInfoVisibility = new ObservableField<>(false);
        mNaviRouteNameVisibility = new ObservableField<>(true);
        mNaviLanesVisibility = new ObservableField<>(false);
        mNaviPreferenceVisibility = new ObservableField<>(false);
        mNaviTmcVisibility = new ObservableField<>(true);
        mNaviEtaVisibility = new ObservableField<>(true);
        mNaviSpeedVisibility = new ObservableField<>(false);
        mNaviSapaVisibility = new ObservableField<>(false);
        mNaviCrossImageVisibility = new ObservableField<>(false);
        mSceneStatus = new ArrayList<>();

    }

    @Override
    protected ClusterNaviGuidanceModel initModel() {
        return new ClusterNaviGuidanceModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        NetworkMonitorUtil.getInstance().addListener(new NetworkMonitorUtil.OnNetStatusChangeListener() {
            @Override
            public void onNetStatusChange(boolean isConnected) {
                // 处理网络状态变化
                // 离线隐藏光柱图，在线显示
                Logger.d(TAG, "onNetStatusChange:" + isConnected);
                mNaviTmcVisibility.set(isConnected);
            }
        });
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 更新场景可见性
     *  sceneType scene type
     *  isVisible is visible
     */
    public void updateSceneVisible(final NaviSceneId sceneType, final boolean isVisible) {
        Logger.d(TAG, "sceneType:" + sceneType + ",isVisible:" + isVisible);
        switch (sceneType) {
            case NAVI_SCENE_3D_CROSS:
            case NAVI_SCENE_2D_CROSS:
                Logger.i(TAG, "路口大图展示状态：" + isVisible);
                mView.onCrossImageInfo(isVisible);
                mNaviCrossImageVisibility.set(isVisible);
                mView.updateViewRadius();
                break;
            case NAVI_SCENE_ETA:
                mNaviEtaVisibility.set(isVisible);
                mView.updateViewRadius();
                break;
            case NAVI_SCENE_LANES:
                mNaviLanesVisibility.set(isVisible);
                mView.updateViewRadius();
                break;
            case NAVI_SCENE_TBT:
                Logger.d(TAG, "TBT面板不会被隐藏");
                break;
            case NAVI_SCENE_VIA_POINT_LIST:
                Logger.i(TAG, "途径点面板展示状态：" + isVisible);
                break;
            case NAVI_SCENE_SERVICE_AREA:
                mNaviSapaVisibility.set(isVisible);
                break;
            case NAVI_SCENE_PARALLEL:
                break;
            case NAVI_SCENE_CONTROL_MORE:
                break;
            case NAVI_SCENE_CONTROL:
                break;
            case NAVI_SCENE_LAST_MILE:

                break;
            case NAVI_SCENE_PREFERENCE:
                mNaviPreferenceVisibility.set(isVisible);
                break;
            case NAVI_SCENE_SPEED:
                mNaviSpeedVisibility.set(isVisible);
                break;
            case NAVI_SCENE_TMC:
                Logger.i(TAG, "NAVI_SCENE_TMC isVisible:" + isVisible + " isNetConnected:" + NetworkMonitorUtil.getInstance().isNetConnected());
                mNaviTmcVisibility.set(isVisible && NetworkMonitorUtil.getInstance().isNetConnected());
                mView.updateViewRadius();
                break;
            case NAVI_SCENE_VIA_DETAIL_INFO:
                mNaviViaInfoVisibility.set(isVisible);
                mView.updateViewRadius();
                break;
            case NAVI_VIA_ARRIVED_POP:
                Logger.d(TAG, "NAVI_VIA_ARRIVED_POP isVisible:" + isVisible);
                break;
            case NAVI_SAPA_DETAIL_INFO:
                Logger.d(TAG, "NAVI_SAPA_DETAIL_INFO isVisible:" + isVisible);
                break;
            case NAVI_DRIVE_REPORT:
                Logger.d(TAG, "NAVI_DRIVE_REPORT isVisible:" + isVisible);
                break;
            case NAVI_CHARGE_TIP:
                Logger.d(TAG, "NAVI_CHARGE_TIP isVisible:" + isVisible);
                break;
            case NAVI_CONTINUE:
                Logger.d(TAG, "NAVI_CONTINUE固定全览：" + NaviPackage.getInstance().getFixedOverViewStatus() + " isVisible:" + isVisible);
                break;
            case NAVI_SUSPEND_CARD:
                Logger.d(TAG, "NAVI_SUSPEND_CARD isVisible:" + isVisible);
                break;
            case NAVI_SUSPEND_CARD_DETAIL:
                Logger.d(TAG, "NAVI_SUSPEND_CARD_DETAIL isVisible:" + isVisible);
                break;
            default:
                break;
        }
    }

    public void startNavigation() {
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
        //修改路线名称
        if (!TextUtils.isEmpty(naviEtaInfo.getCurRouteName())) {
            mView.updateRouteName(naviEtaInfo.getCurRouteName());
        }
        //更新导航信息
        mView.onNaviInfo(naviEtaInfo);
    }

    /**
     * 更新路线名称
     *
     * @param naviEtaInfo navi eta info
     */
    private void updateRouteName(final NaviEtaInfo naviEtaInfo) {

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
        mView.onLaneInfo(isShowLane, laneInfo);
    }

    /**
     * 沉浸态状态改变回调
     *
     * @param currentImersiveStatus current immersive status
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "onImmersiveStatusChange固定全览：" + NaviPackage.getInstance().getFixedOverViewStatus() + " currentImersiveStatus:" + currentImersiveStatus);
        mNaviRouteNameVisibility.set(NaviPackage.getInstance().getFixedOverViewStatus() || currentImersiveStatus == ImersiveStatus.IMERSIVE);
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
     * 途经点通过回调
     *
     * @param viaIndex via index
     */
    public void onUpdateViaPass(final long viaIndex) {
        mView.onUpdateViaPass(viaIndex);
    }
    public void onUpdateViaList(boolean isShow) {
        mView.onUpdateTMCLightBarAutoAdd(isShow);
    }
    public ArrayList<Integer> getSceneStatus() {
        return mSceneStatus;
    }
}
