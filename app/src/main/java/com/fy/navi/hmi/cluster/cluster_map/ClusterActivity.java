package com.fy.navi.hmi.cluster.cluster_map;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.cluster.ClusterViewModel;
import com.fy.navi.hmi.cluster.cluster_navi_info.NaviClusterGuidanceFragment;
import com.fy.navi.hmi.databinding.ActivityClusterBinding;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseActivity;

public class ClusterActivity extends BaseActivity<ActivityClusterBinding, ClusterViewModel>   {

    /**
     * TAG
     */
    private static final String TAG = "ClusterActivityTAG";
    /**
     * 通过广播关闭Activity
     */
    private BroadcastReceiver mBroadcastReceiver;
    @Override
    public int onLayoutId() {
        return R.layout.activity_cluster;
    }

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.CLUSTER_MAP.name();
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Logger.d(TAG, "onCreate");
        //注册关闭Activity广播
        registerBroadcastReceiver();
        //添加 ClusterNaviInfoFragment 导航信息Fragment
        //addNaviInfoFragment();
    }

    @Override
    protected void onStart() {
        super.onStart();
        Logger.d(TAG, "onStart");
    }
    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        unregisterReceiver(mBroadcastReceiver);
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(TAG, "onInitView");
        mViewModel.loadMapView();
    }

    @Override
    public void onInitData() {
        Logger.d(TAG, "onInitData");
    }

    public IBaseScreenMapView getMapView() {
        Logger.d(TAG, "getMapView");
        return mBinding.clusterMapview;
    }

    /**
     * 注册关闭Activity广播
     */
    @SuppressLint({"WrongConstant", "UnspecifiedRegisterReceiverFlag"})
    private void registerBroadcastReceiver() {
        Logger.d(TAG, "registerBroadcastReceiver");
        mBroadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                Logger.d(TAG, "onReceive: " + intent.getAction());
                if ("com.fy.navi.hmi.cluster_map.ClusterActivity".equals(intent.getAction())) {
                    Logger.d(TAG, "onReceive: com.fy.navi.hmi.cluster.ClusterActivity");
                    ClusterActivity.this.finish(); // 关闭当前 Activity
                }
            }
        };
        registerReceiver(mBroadcastReceiver, new IntentFilter("com.fy.navi.hmi.cluster_map.ClusterActivity"));

    }
    /**
     * 添加 ClusterNaviInfoFragment 导航信息Fragment
     */
    private void addNaviInfoFragment() {
        Logger.d(TAG, "addNaviInfoFragment");
        NaviClusterGuidanceFragment fragment = NaviClusterGuidanceFragment.newInstance();
        getSupportFragmentManager().beginTransaction()
                .add(R.id.cluster_frameLayout, fragment)
                .commit();
    }
}

