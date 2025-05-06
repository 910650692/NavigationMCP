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
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/27
 * Description: [在这里描述文件功能]
 */
public class BaseSmallCardMapViewModel extends BaseViewModel<SmallCardMapFragment, SmallCardMapModel> {
    private final String TAG = "BaseSmallCardMapViewModel";
    public ObservableField<Boolean> naviBarVisibility;
    public ObservableField<Boolean> naviUiVisibility;//tmc
    public ObservableField<Boolean> cruiseUiVisibility;//tmc
    public ObservableField<Boolean> mPlaceHolderVisibility;//占位图
    public ObservableField<Boolean> mCruiseEgleVisibility; // 巡航电子眼
    private NaviEtaInfo mNaviEtaInfo;

    public BaseSmallCardMapViewModel(@NonNull Application application) {
        super(application);
        naviBarVisibility = new ObservableField<>(true);
        naviUiVisibility = new ObservableField<>(true);
        cruiseUiVisibility = new ObservableField<>(true);
        mPlaceHolderVisibility = new ObservableField<>(true);
        mCruiseEgleVisibility = new ObservableField<>(false);
        mNaviEtaInfo = new NaviEtaInfo();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        naviBarVisibility.set(!TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING));
        naviUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING));
        cruiseUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.CRUISE));
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    protected SmallCardMapModel initModel() {
        return new SmallCardMapModel();
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

    public void onNaviStatusChanged(@NaviStatus.NaviStatusType String status) {
        naviBarVisibility.set(!TextUtils.equals(status, NaviStatus.NaviStatusType.NAVING));
        naviUiVisibility.set(TextUtils.equals(status, NaviStatus.NaviStatusType.NAVING));
        cruiseUiVisibility.set(TextUtils.equals(status, NaviStatus.NaviStatusType.CRUISE));
    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        naviBarVisibility.set(false);
        updateRouteName(naviEtaInfo);
        mNaviEtaInfo = naviEtaInfo;
        mView.onNaviInfo(naviEtaInfo);
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
    }

    public void updateCruiseCameraInfo(CruiseInfoEntity cruiseInfoEntity) {
        mView.updateCruiseCameraInfo(cruiseInfoEntity);
        mCruiseEgleVisibility.set(cruiseInfoEntity != null);
    }

    public void updateCruiseLanInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
        mView.updateCruiseLanInfo(isShowLane, laneInfoEntity);
    }

    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
        mView.onUpdateTMCLightBar(naviTmcInfo);
    }
}
