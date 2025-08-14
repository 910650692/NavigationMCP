package com.sgm.navi.hmi.rearscreen;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseRearScreenViewModel extends BaseViewModel<RearScreenActivity, RearScreenModel> {
    private static final String TAG = BaseRearScreenViewModel.class.getSimpleName();
    public ObservableField<String> currentRoadName = new ObservableField<>(); // 当前路名
    public ObservableField<String> destinationName = new ObservableField<>(); // 目的地名称
    public ObservableField<String> remainDistanceTime = new ObservableField<>();// 剩余距离和时间
    public ObservableField<String> arriveTime = new ObservableField<>(); // 到达时间
    public ObservableField<String> arrivalDay = new ObservableField<>(); // 到达天数
    // 道路名是否显示
    public final ObservableField<Boolean> roadNameVisible = new ObservableField<>(false);
    // ETA是否显示
    public final ObservableField<Boolean> etaInfoVisible = new ObservableField<>(false);
    // 是否显示预览按钮
    public final ObservableField<Boolean> isShowPreviewBtn = new ObservableField<>(false);
    // 是全览还是非全览
    public final ObservableField<Boolean> isPreview = new ObservableField<>(false);

    /**
     * 看全览
     */
    public Action showOrClosePreview = () -> {
        Logger.i(TAG, "showOrClosePreview:" + isPreview.get());
        if (Boolean.TRUE.equals(isPreview.get())) {
            updatePreviewStatus(false);
            mModel.closePreview();
        } else {
            updatePreviewStatus(true);
            mModel.showPreview();
//            startPreviewSchedule();
        }
    };

    public BaseRearScreenViewModel(@NonNull Application application) {
        super(application);
        Logger.d(TAG, "BaseRearScreenViewModel");
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");
        initData();
    }

    private void initData() {
        updatePreviewStatus(false);
        boolean isNavigating = isNavigating();
        updateNaviStatus(isNavigating);
        if (isNavigating) {
            updateEta(mModel.getCurrentNaviEtaInfo());
        }
    }

    @Override
    protected RearScreenModel initModel() {
        Logger.d(TAG, "initModel");
        return new RearScreenModel();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        MapPackage.getInstance().unBindMapView(mView.getMapView());
    }

    public void loadMapView() {
        mView.bindMapView();
    }

    public IBaseScreenMapView getMapView() {
        Logger.d(TAG, "getMapView");
        return mView.getMapView();
    }

    public void updateEta(NaviEtaInfo naviEtaInfo) {
        Logger.d(TAG, "updateEta", naviEtaInfo);
        updateRoadName(naviEtaInfo.getCurRouteName());
        destinationName.set(mModel.getDestinationName());
        int distance = naviEtaInfo.getRemainDist();
        int time = naviEtaInfo.getRemainTime();

        if (distance <= 0 && time <= 0) {
            Logger.d(TAG, "Distance and time are both zero, skipping update");
            return;
        }
        String strArriveDay = TimeUtils.getArriveDay(time);
        String strArriveTime = TimeUtils.getArriveTime(AppCache.getInstance().getMContext(), time);
        String strRemainInfo = TimeUtils.getRemainInfo(AppCache.getInstance().getMContext(), distance, time);
        if (!TextUtils.isEmpty(strArriveTime)) {
            arriveTime.set(ConvertUtils.digitToBold(strArriveTime).toString());
        }
        if (!TextUtils.isEmpty(strArriveDay)) {
            arrivalDay.set(strArriveDay);
            mView.setDayShowOrHide(true);
        } else {
            mView.setDayShowOrHide(false);
        }
        remainDistanceTime.set(ConvertUtils.digitToBold(strRemainInfo).toString());
        mView.onNaviInfo(naviEtaInfo);
    }

    /**
     * 更新TMC路况信息
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, final boolean isShow) {
        mView.onUpdateTMCLightBar(naviTmcInfo, isShow);
    }

    private void updateRoadName(String curRoadName) {
        Logger.d(TAG, "updateRoadName---curRoadName:" + curRoadName);
        if (!ConvertUtils.isEmpty(curRoadName)) {
            roadNameVisible.set(true);
            currentRoadName.set(curRoadName);
        } else {
            roadNameVisible.set(false);
            currentRoadName.set("");
        }
    }

    public void updatePreviewStatus(boolean isPreviewStatus) {
        isPreview.set(isPreviewStatus);
        mView.updatePreviewStatus(isPreviewStatus);
    }

    public void updateNaviStatus(boolean isNavigating) {
        Logger.d(TAG, "updateNaviStatus---isNavigating:" + isNavigating);
        etaInfoVisible.set(isNavigating);
        roadNameVisible.set(isNavigating);
        isShowPreviewBtn.set(isNavigating);
    }

    public void registerRearScreenMap() {
        Logger.d(TAG, "registerRearScreenMap");
        mModel.registerRearScreenMap();
    }

    public boolean isNavigating() {
        return mModel.isNavigating();
    }

    public int[] getLogoPosition() {
        return mView.getCarSelfPosition();
    }
}
