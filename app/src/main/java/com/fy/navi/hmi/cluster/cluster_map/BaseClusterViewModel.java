package com.fy.navi.hmi.cluster.cluster_map;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseClusterViewModel extends  BaseViewModel<ClusterActivity, ClusterModel> {
    private final String TAG = "BaseClusterViewModel";

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
}
