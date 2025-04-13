package com.fy.navi.hmi.launcher;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description TODO 功能待完善，代码可以参照主图
 * @Author yaWei
 * @date 2025/2/18
 */
public class BaseLauncherSmallCardViewModel extends BaseViewModel<MapLauncherSmallCardActivity, LauncherSmallCardModel> {
    private static final String TAG = "BaseLauncherSmallCardViewModel";
    public ObservableField<Boolean> naviBarVisibility;
    public ObservableField<Boolean> naviUiVisibility;//tmc
    public ObservableField<Boolean> cruiseUiVisibility;//tmc
    private NaviEtaInfo mNaviEtaInfo;

    public BaseLauncherSmallCardViewModel(@NonNull Application application) {
        super(application);
        naviBarVisibility = new ObservableField<>(true);
        naviUiVisibility = new ObservableField<>(true);
        cruiseUiVisibility = new ObservableField<>(true);
        mNaviEtaInfo = new NaviEtaInfo();
    }

    @Override
    protected LauncherSmallCardModel initModel() {
        return new LauncherSmallCardModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        LauncherManager.getInstance().startInitService();
        naviBarVisibility.set(!TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING));
        naviUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING));
        cruiseUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.CRUISE));
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public Action goHome = () -> {
        Logger.d(TAG, "goHome");
        LauncherManager.getInstance().startMapActivity(INaviConstant.OpenIntentPage.GO_HOME);
    };

    public Action goCompany = () -> {
        Logger.d(TAG, "goCompany");
        LauncherManager.getInstance().startMapActivity(INaviConstant.OpenIntentPage.GO_COMPANY);
    };

    public Action goSearch = () -> {
        Logger.d(TAG, "goSearch");
        LauncherManager.getInstance().startMapActivity(INaviConstant.OpenIntentPage.SEARCH_PAGE);
    };

    public void loadMapView() {
        mModel.loadMapView();
    }

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }

    public void setMapCenterInScreen(int frameLayoutWidth) {
        mModel.setMapCenterInScreen(frameLayoutWidth);
//        mainBTNVisibility.set(false);
    }

    public void resetMapCenterInScreen() {
        mModel.resetMapCenterInScreen(mView.getMapView());
    }

    public void onNaviStatusChanged(@NaviStatus.NaviStatusType String status) {
        naviBarVisibility.set(!TextUtils.equals(status, NaviStatus.NaviStatusType.NAVING));
        naviUiVisibility.set(TextUtils.equals(status, NaviStatus.NaviStatusType.NAVING));
        cruiseUiVisibility.set(TextUtils.equals(status, NaviStatus.NaviStatusType.CRUISE));
    }

    public void updateSceneVisible(int sceneType, boolean isVisible) {
        Logger.i(TAG, "sceneType:" + sceneType, "isVisible:" + isVisible);
        switch (sceneType) {
            case NaviConstant.NaviSceneType.SCENE_TBT:
                break;
            case NaviConstant.NaviSceneType.SCENE_LANES:
                break;
            case NaviConstant.NaviSceneType.SCENE_PARK_LIST:
//                naviParkingListVisibility.set(isVisible);
                break;
            case NaviConstant.NaviSceneType.SCENE_VIA_INFO:
//                naviViaInfoVisibility.set(isVisible);
                break;
            case NaviConstant.NaviSceneType.SCENE_LAST_MILE:
//                naviLastMileVisibility.set(isVisible);
                break;
            case NaviConstant.NaviSceneType.SCENE_PARALLEL:
//                naviParallelVisibility.set(isVisible);
                break;
            case NaviConstant.NaviSceneType.SCENE_PREFERENCE:
//                naviPreferenceVisibility.set(isVisible);
                break;
            case NaviConstant.NaviSceneType.SCENE_SPEED:
//                naviSpeedVisibility.set(isVisible);
                break;
            case NaviConstant.NaviSceneType.SCENE_SAPA:
//                naviSapaVisibility.set(isVisible);
                break;
            case NaviConstant.NaviSceneType.SCENE_CROSS_IMAGE:
//                naviCrossImageVisibility.set(isVisible);
//                if (isVisible) {
//                    naviTmcVisibility.set(false);
//                } else {
//                    naviTmcVisibility.set(true);
//                }
                break;
            case NaviConstant.NaviSceneType.SCENE_CONTROL:
//                naviControlVisibility.set(isVisible);
                break;
            default:
                break;
        }
    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        naviBarVisibility.set(false);
        updateRouteName(naviEtaInfo);
        mNaviEtaInfo = naviEtaInfo;
        mView.onNaviInfo(naviEtaInfo);
    }

    public void onCruiseInfo() {

    }

    private void updateRouteName(NaviEtaInfo naviEtaInfo) {
        if (!TextUtils.isEmpty(naviEtaInfo.getCurRouteName())) {
            mView.updateRouteName(naviEtaInfo.getCurRouteName());
        }
    }

    public Action stopNavi = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_END_MANUAL)
        public void call() {
            mModel.stopNavi();

            BuryProperty buryProperty = new BuryProperty.Builder()
                    .setParams(BuryConstant.ProperType.BURY_KEY_REMAINING_TIME, TimeUtils.getArriveTime(mApplication.getApplicationContext(), mNaviEtaInfo.getAllTime()))
                    .setParams(BuryConstant.ProperType.BURY_KEY_TRIP_DISTANCE, TimeUtils.getRemainInfo(mApplication.getApplicationContext(), mNaviEtaInfo.getAllDist(), mNaviEtaInfo.getAllTime()))
                    .build();
            BuryPointController.getInstance().setBuryProps(buryProperty);
            mNaviEtaInfo = new NaviEtaInfo();
        }
    };

    public void naviArriveOrStop() {
        naviUiVisibility.set(false);
        naviBarVisibility.set(true);
        mModel.resetMapCenterInScreen(mView.getMapView());
    }

    public void updateCruiseCameraInfo(CruiseInfoEntity cruiseInfoEntity) {
        mView.updateCruiseCameraInfo(cruiseInfoEntity);
    }

    public void updateCruiseLanInfo(LaneInfoEntity laneInfoEntity) {
        mView.updateCruiseLanInfo(laneInfoEntity);
    }

    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
        mView.onUpdateTMCLightBar(naviTmcInfo);
    }
}
