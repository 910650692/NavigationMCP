package com.fy.navi.hmi.cluster.cluster_map;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseClusterViewModel extends  BaseViewModel<ClusterActivity, ClusterModel> {
    private static final String TAG = "BaseClusterViewModel";

    public BaseClusterViewModel(@NonNull Application application) {
        super(application);
        Logger.d(TAG, "BaseClusterViewModel");
    }
    @Override
    protected ClusterModel initModel() {
        Logger.d(TAG, "initModel");
        return new ClusterModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");
    }

    public void loadMapView() {
        Logger.d(TAG, "loadMapView");
        mModel.loadMapView();
    }

    public IBaseScreenMapView getMapView() {
        Logger.d(TAG, "getMapView");
        return mView.getMapView();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
    }

    public void updateEta(NaviEtaInfo naviEtaInfo) {
        Logger.d(TAG, "updateEta");
        if (naviEtaInfo == null || !checkNaviInfoPanelLegal(naviEtaInfo)) {
            return;
        }
        //更新ETA信息
        mView.updateEta(naviEtaInfo.getAllDist(), naviEtaInfo.getAllTime());
    }
    public void updateRouteName(String curRouteName){
        Logger.d(TAG, "updateRouteName");
        //更新当前路线名字信息
        if (!curRouteName.isEmpty()){
            Logger.d(TAG, "updateRouteName"+curRouteName);
            mView.updateRouteName(curRouteName);
        }

    }

    public void updateNaviStatus(String naviStatus) {
        mView.setVS(naviStatus);
    }

    /**
     * 检查NaviInfoPanel是否合法
     * @param naviinfo naviinfo
     * @return boolean
     */
    public static boolean checkNaviInfoPanelLegal(final NaviEtaInfo naviinfo) {
        if (naviinfo == null) {
            Logger.d(TAG, "checkNaviInfoPanelLegal naviInfo null");
            return false;
        }
        if (naviinfo.NaviInfoData == null || naviinfo.NaviInfoData.isEmpty()) {
            Logger.d(TAG, "checkNaviInfoPanelLegal naviinfo.NaviInfoData null");
            return false;
        }
        if (naviinfo.NaviInfoFlag > naviinfo.NaviInfoData.size() - 1) {
            Logger.d(TAG, "checkNaviInfoPanelLegal naviinfo.NaviInfoFlag length out bound!");
            return false;
        }
        if (naviinfo.NaviInfoData.get(naviinfo.NaviInfoFlag) == null) {
            Logger.d(TAG, "checkNaviInfoPanelLegal naviinfo.NaviInfoData[naviinfo.NaviInfoFlag] null");
            return false;
        }
        return true;
    }
}
