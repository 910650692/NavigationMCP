package com.fy.navi.hmi.launcher;

import android.app.ActivityOptions;
import android.app.Application;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.search.PoiInfoEntity;
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
    public ObservableField<Boolean> mPlaceHolderVisibility;//占位图
    public ObservableField<Boolean> mCruiseEgleVisibility; // 巡航电子眼
    private NaviEtaInfo mNaviEtaInfo;

    public BaseLauncherSmallCardViewModel(@NonNull Application application) {
        super(application);
        naviBarVisibility = new ObservableField<>(true);
        naviUiVisibility = new ObservableField<>(true);
        cruiseUiVisibility = new ObservableField<>(true);
        mPlaceHolderVisibility = new ObservableField<>(true);
        mCruiseEgleVisibility = new ObservableField<>(false);
        mNaviEtaInfo = new NaviEtaInfo();
    }

    @Override
    protected LauncherSmallCardModel initModel() {
        return new LauncherSmallCardModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        naviBarVisibility.set(!TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING));
        naviUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING));
        cruiseUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.CRUISE));
    }

    public Action goHome = () -> {
        Logger.d(TAG, "goHome");
        startMapActivity(INaviConstant.OpenIntentPage.GO_HOME);
    };

    public Action goCompany = () -> {
        Logger.d(TAG, "goCompany");
        startMapActivity(INaviConstant.OpenIntentPage.GO_COMPANY);
    };

    public Action goSearch = () -> {
        Logger.d(TAG, "goSearch");
        startMapActivity(INaviConstant.OpenIntentPage.SEARCH_PAGE);
    };

    public void loadMapView() {
        mModel.loadMapView();
    }

    public void onNaviStatusChanged(@NaviStatus.NaviStatusType String status) {
        naviBarVisibility.set(!TextUtils.equals(status, NaviStatus.NaviStatusType.NAVING));
        naviUiVisibility.set(TextUtils.equals(status, NaviStatus.NaviStatusType.NAVING));
        cruiseUiVisibility.set(TextUtils.equals(status, NaviStatus.NaviStatusType.CRUISE));
    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        naviBarVisibility.set(false);
        naviUiVisibility.set(true);
        updateRouteName(naviEtaInfo);
        mNaviEtaInfo = naviEtaInfo;
        mView.onNaviInfo(naviEtaInfo);
    }

    private void updateRouteName(NaviEtaInfo naviEtaInfo) {
        if (!TextUtils.isEmpty(naviEtaInfo.getCurRouteName())) {
//            mView.updateRouteName(naviEtaInfo.getCurRouteName());
        }
    }

    public Action stopNavi = () -> mModel.stopNavi();

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

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }

    /***
     * 启动Navi_App
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_WIDGET_ENTERAPP)
    public void startMapActivity(int pageCode, @Nullable PoiInfoEntity poiInfo) {
        Logger.i(TAG, "startMapActivity:" + pageCode);
        Intent intent = new Intent(AppCache.getInstance().getMContext(), MapActivity.class);
        Bundle bundle = new Bundle();
        bundle.putInt(INaviConstant.PAGE_EXTRA, pageCode);
        if (poiInfo != null) {
            bundle.putParcelable(INaviConstant.POI_INFO_EXTRA, poiInfo);
        }
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtras(bundle);
        ActivityOptions options = ActivityOptions.makeBasic();
        options.setLaunchDisplayId(0);
        AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
    }

    /***
     * 启动Navi_App
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_WIDGET_ENTERAPP)
    public void startMapActivity(int pageCode) {
        Logger.i(TAG, "startMapActivity:" + pageCode);
        Intent intent = new Intent(AppCache.getInstance().getMContext(), MapActivity.class);
        intent.putExtra(INaviConstant.PAGE_EXTRA, pageCode);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        ActivityOptions options = ActivityOptions.makeBasic();
        options.setLaunchDisplayId(0);
        AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
    }
}
