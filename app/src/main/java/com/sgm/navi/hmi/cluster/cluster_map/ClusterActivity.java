package com.sgm.navi.hmi.cluster.cluster_map;


import static android.view.View.VISIBLE;

import android.content.res.Configuration;

import androidx.annotation.NonNull;

import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.cluster.ClusterViewModel;
import com.sgm.navi.hmi.databinding.ActivityClusterBinding;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.view.SkinConstraintLayout;

public class ClusterActivity extends BaseActivity<ActivityClusterBinding, ClusterViewModel> {
    private static final String TAG = "ClusterActivityTAG";

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.CLUSTER_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_cluster;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(TAG, "onInitView");
        mViewModel.registerClusterMap();
    }

    @Override
    public void onInitData() {
        Logger.d(TAG, "onInitData");
    }

    @Override
    protected void onResume() {
        super.onResume();
        Logger.d(TAG, "onResume");
        getRootView().setVisibility(VISIBLE);
        mViewModel.remainingMileageConstraintLayoutVisibility.set(NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING));
        mViewModel.routeNameConstraintLayoutVisibility.set(NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING));
    }

    @Override
    protected void onPause() {
        super.onPause();
        Logger.d(TAG, "onPause");
    }

    @Override
    protected void onStop() {
        super.onStop();
        Logger.d(TAG, "onStop");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        Logger.d(TAG, "onConfigurationChanged");
        updateMapThemeType();
    }

    private void updateMapThemeType() {
        boolean nightModeEnabled = ThemeUtils.INSTANCE.isNightModeEnabled(this);
        Logger.d(TAG, "updateMapThemeType:nightModeEnabled:" + nightModeEnabled);
        ThemeType colorMode = nightModeEnabled ? ThemeType.NIGHT : ThemeType.DAY;
        MapAdapter.getInstance().updateUiStyle(MapType.CLUSTER_MAP, colorMode);
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.clusterMapview;
    }

    @Override
    protected void onStart() {
        super.onStart();
        Logger.d(TAG, "onStart");
    }

    public SkinConstraintLayout getRootView() {
        return mBinding.cluster;
    }

    public void bindMapView() {
        mBinding.clusterMapview.post(() -> {
            MapPackage.getInstance().bindMapView(mBinding.clusterMapview);
            LayerAdapter.getInstance().initLayer(MapType.CLUSTER_MAP);
        });
    }
}