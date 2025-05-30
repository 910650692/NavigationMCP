package com.fy.navi.hmi.cluster.cluster_map;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseClusterViewModel extends BaseViewModel<ClusterActivity, ClusterModel> {

    private static final String TAG = "BaseClusterViewModel";

    public BaseClusterViewModel(@NonNull Application application) {
        super(application);
        Logger.d(TAG, "BaseClusterViewModel initialized");
    }

    @Override
    protected ClusterModel initModel() {
        Logger.d(TAG, "Initializing ClusterModel");
        return new ClusterModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate called");
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy called");
    }

    // ========= 生命周期相关 =========

    public void loadMapView() {
        Logger.d(TAG, "Loading map view");
        mModel.loadMapView();
    }

    public IBaseScreenMapView getMapView() {
        Logger.d(TAG, "Getting map view");
        return mView.getMapView();
    }

    // ========= Navi ETA 更新 =========

    public void updateEta(NaviEtaInfo naviEtaInfo) {
        Logger.d(TAG, "Updating ETA info");

        if (naviEtaInfo == null || !checkNaviInfoPanelLegal(naviEtaInfo)) {
            Logger.d(TAG, "ETA info invalid or illegal");
            return;
        }

        int distance = naviEtaInfo.getAllDist();
        int time = naviEtaInfo.getAllTime();

        if (distance <= 0 && time <= 0) {
            Logger.d(TAG, "Distance and time are both zero, skipping update");
            return;
        }

        mView.updateEta(distance, time);
    }

    // ========= Route Name 更新 =========

    public void updateRouteName(String curRouteName) {
        Logger.d(TAG, "Updating route name");

        if (TextUtils.isEmpty(curRouteName)) {
            Logger.d(TAG, "Route name is empty, skipping update");
            return;
        }

        mView.updateRouteName(curRouteName);
    }

    // ========= 导航状态同步 =========

    public void updateNaviStatus(String naviStatus) {
        Logger.d(TAG, "Updating navigation status: " + naviStatus);
        mView.setVS(naviStatus);
    }

    /**
     * 检查导航信息是否合法
     *
     * @param naviinfo 导航信息对象
     * @return boolean 是否合法
     */
    public static boolean checkNaviInfoPanelLegal(final NaviEtaInfo naviinfo) {
        if (naviinfo == null) {
            Logger.d(TAG, "Navi info is null");
            return false;
        }
        if (naviinfo.NaviInfoData == null || naviinfo.NaviInfoData.isEmpty()) {
            Logger.d(TAG, "Navi info data is null or empty");
            return false;
        }
        if (naviinfo.NaviInfoFlag >= naviinfo.NaviInfoData.size()) {
            Logger.d(TAG, "Navi info flag out of bounds");
            return false;
        }
        if (naviinfo.NaviInfoData.get(naviinfo.NaviInfoFlag) == null) {
            Logger.d(TAG, "Navi info data at flag index is null");
            return false;
        }
        return true;
    }
}
