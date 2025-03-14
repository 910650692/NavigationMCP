package com.fy.navi.hmi.navi;

import android.app.Application;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.scene.api.route.ISceneRoutePreferenceCallBack;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.List;


/**
 * description 导航Fragment视图模型
 */
public class BaseNaviGuidanceViewModel extends BaseViewModel<NaviGuidanceFragment, NaviGuidanceModel> implements ISceneRoutePreferenceCallBack {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    public ObservableField<Boolean> naviLanesVisibility;//车道线
    public ObservableField<Boolean> naviViaListVisibility;//途径点列表
    public ObservableField<Boolean> naviParkingListVisibility;//停车场列表
    public ObservableField<Boolean> naviViaInfoVisibility;//显示途径点信息
    public ObservableField<Boolean> naviLastMileVisibility;//最后一公里
    public ObservableField<Boolean> naviRouteNameVisibility;//当前路名
    public ObservableField<Boolean> naviParallelVisibility;//平行路
    public ObservableField<Boolean> naviPreferenceVisibility;//路线偏好
    public ObservableField<Boolean> naviTmcVisibility;//tmc
    public ObservableField<Boolean> naviEtaVisibility;//eta
    public ObservableField<Boolean> naviSpeedVisibility;//限速
    public ObservableField<Boolean> naviSapaVisibility;//Sapa
    public ObservableField<Boolean> naviCrossImageVisibility;//路口大图
    public ObservableField<Boolean> naviControlVisibility;//control
    public ObservableField<Boolean> naviViaArrivedPopVisibility;//途经点到达确认弹窗
    private boolean mIsShowLane = false;

    public BaseNaviGuidanceViewModel(@NonNull Application application) {
        super(application);
        naviViaInfoVisibility = new ObservableField<>(false);
        naviLastMileVisibility = new ObservableField<>(false);
        naviRouteNameVisibility = new ObservableField<>(true);
        naviParallelVisibility = new ObservableField<>(false);
        naviLanesVisibility = new ObservableField<>(false);
        naviViaListVisibility = new ObservableField<>(false);
        naviParkingListVisibility = new ObservableField<>(false);
        naviPreferenceVisibility = new ObservableField<>(false);
        naviTmcVisibility = new ObservableField<>(true);
        naviEtaVisibility = new ObservableField<>(true);
        naviSpeedVisibility = new ObservableField<>(false);
        naviSapaVisibility = new ObservableField<>(false);
        naviCrossImageVisibility = new ObservableField<>(false);
        naviControlVisibility = new ObservableField<>(false);
        naviViaArrivedPopVisibility = new ObservableField<>(false);
    }

    @Override
    protected NaviGuidanceModel initModel() {
        return new NaviGuidanceModel();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    //显示/隐藏 添加途径点页面
    public Action naviAddVia = this::onSwitchViaList;

    public void onSwitchViaList() {
        Boolean b = naviViaListVisibility.get();
        Logger.i("lvww", "b -> " + b);
        boolean aa = Boolean.FALSE.equals(b);
        Logger.i("lvww", "aa -> " + aa);
        updateViaListState(aa);
//        naviViaListVisibility.set(Boolean.FALSE.equals(b));
//
//        Boolean lanes = naviLanesVisibility.get();
//        if (Boolean.TRUE.equals(lanes)) {
//            naviLanesVisibility.set(false);
//        } else if (Boolean.FALSE.equals(lanes) && mIsShowLane) {
//            naviLanesVisibility.set(true);
//        }
        List<NaviViaEntity> viaList = mModel.getViaList();
//        Boolean viaInfo = naviViaInfoVisibility.get();
//        if (Boolean.TRUE.equals(viaInfo)) {
//            naviViaInfoVisibility.set(false);
//        } else if (Boolean.FALSE.equals(viaInfo) && viaList.size() > 1) {
//            naviViaInfoVisibility.set(true);
//        }
//        naviTmcVisibility.set(b);
//        naviEtaVisibility.set(b);
        mView.showNaviViaList(Boolean.FALSE.equals(b) ? viaList : null);
    }

    public void updateSceneVisible(NaviSceneId sceneType, boolean isVisible) {
        Logger.i(TAG, "sceneType:" + sceneType + ",isVisible:" + isVisible);
        switch (sceneType) {
            case NAVI_SCENE_3D_CROSS:
                break;
            case NAVI_SCENE_2D_CROSS:
                naviCrossImageVisibility.set(isVisible);
                break;
            case NAVI_SCENE_ETA:
                break;
            case NAVI_SCENE_LANES:
                break;
            case NAVI_SCENE_TBT:
                break;
            case NAVI_SCENE_VIA_POINT_FOLD:
            case NAVI_SCENE_VIA_POINT_UNFOLD:
                naviViaListVisibility.set(isVisible);
                break;
            case NAVI_SCENE_SERVICE_AREA:
                naviSapaVisibility.set(isVisible);
                break;
            case CARD_CONTINUE_NAVI_BTN:
                break;
            case NAVI_SCENE_PARALLEL:
                naviParallelVisibility.set(isVisible);
                break;
            case NAVI_SCENE_PARK_LIST:
                naviParkingListVisibility.set(isVisible);
                break;
            case NAVI_SCENE_CONTROL:
                naviControlVisibility.set(isVisible);
                break;
            case NAVI_SCENE_LAST_MILE:
                naviLastMileVisibility.set(isVisible);
                break;
            case NAVI_SCENE_PREFERENCE:
                naviPreferenceVisibility.set(isVisible);
                break;
            case NAVI_SCENE_SPEED:
                naviSpeedVisibility.set(isVisible);
                break;
            case NAVI_SCENE_TMC:
                break;
            case NAVI_SCENE_VIA_DETAIL_INFO:
                naviViaInfoVisibility.set(isVisible);
                break;
            case NAVI_VIA_ARRIVED_POP:
                naviViaArrivedPopVisibility.set(isVisible);
            default:
                break;
        }
    }

    public void startNavigation(Bundle bundle) {
        mModel.startNavigation(bundle);
        mView.startNavigation();
    }

    public void onNaviSpeedCameraInfo(SpeedOverallEntity speedCameraInfo) {
        mView.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
        mView.onNaviSAPAInfo(sapaInfoEntity);
    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        updateRouteName(naviEtaInfo);
        mView.onNaviInfo(naviEtaInfo);
    }

    private void updateRouteName(NaviEtaInfo naviEtaInfo) {
        if (!TextUtils.isEmpty(naviEtaInfo.curRouteName)) {
            mView.updateRouteName(naviEtaInfo.curRouteName);
        }
    }

    public void onNaviStop() {
        mView.onNaviStop();
        closeAllFragment();
    }

    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        mView.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
        mView.onUpdateTMCLightBar(naviTmcInfo);
    }

    public void onManeuverInfo(NaviManeuverInfo info) {
        mView.onManeuverInfo(info);
    }

    public void onNaviArrive(long traceId, int naviType) {
        mView.onNaviArrive(traceId, naviType);
    }

    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfo) {
        mIsShowLane = isShowLane;
        naviLanesVisibility.set(isShowLane);
        mView.onLaneInfo(isShowLane, laneInfo);
    }

    public void onImmersiveStatusChange(ImersiveStatus currentImersiveStatus) {
        naviRouteNameVisibility.set(currentImersiveStatus == ImersiveStatus.IMERSIVE);
        mView.onImmersiveStatusChange(currentImersiveStatus);
    }

    public void addSceneCallback(ISceneCallback sceneCallback) {
        mView.addSceneCallback(sceneCallback);
    }

    public void updateViaListState(boolean isExpand) {
        mView.updateViaListState(isExpand);
    }

    public void showNaviPreferenceScene() {
        naviPreferenceVisibility.set(true);
    }

    @Override
    public void onRoutePreferenceChange(String text, boolean isFirstChange) {
        Logger.d(TAG, "text：" + text + ",isFirstChange：" + isFirstChange);
        if (!isFirstChange) {
            naviPreferenceVisibility.set(false);
            mModel.onRoutePreferenceChange();
        }
    }

    public void onUpdateViaPass(long viaIndex) {
        mView.onUpdateViaPass(viaIndex);
    }

    public void notifyDeleteViaPointResult(boolean result, NaviViaEntity entity) {
        mView.notifyDeleteViaPointResult(result, entity);
    }
}
