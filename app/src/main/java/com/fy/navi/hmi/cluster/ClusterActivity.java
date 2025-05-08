package com.fy.navi.hmi.cluster;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Build;
import android.os.Bundle;

import androidx.activity.EdgeToEdge;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import com.android.utils.log.Logger;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityClusterBinding;
import com.fy.navi.hmi.databinding.ActivityLauncherDeskBinding;
import com.fy.navi.hmi.launcher.BaseLauncherDeskViewModel;
import com.fy.navi.hmi.launcher.LauncherManager;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviTmcInfo;
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
        //注册关闭Activity广播
        registerBroadcastReceiver();
        //添加 ClusterNaviInfoFragment 导航信息Fragment
        addNaviInfoFragment();
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
                if ("com.fy.navi.hmi.cluster.ClusterActivity".equals(intent.getAction())) {
                    Logger.d(TAG, "onReceive: com.fy.navi.hmi.cluster.ClusterActivity");
                    ClusterActivity.this.finish(); // 关闭当前 Activity
                }
            }
        };
        registerReceiver(mBroadcastReceiver, new IntentFilter("com.fy.navi.hmi.cluster.ClusterActivity"));

    }
    /**
     * 添加 ClusterNaviInfoFragment 导航信息Fragment
     */
    private void addNaviInfoFragment() {
        Logger.d(TAG, "addNaviInfoFragment");
        // 添加 ClusterNaviInfoFragment
        ClusterNaviInfoFragment fragment = ClusterNaviInfoFragment.newInstance();
        getSupportFragmentManager().beginTransaction()
                .add(R.id.cluster_frameLayout, fragment)
                .commit();
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
        mViewModel.loadMapView();
    }

    @Override
    public void onInitData() {

    }

    @Override
    protected void onMoveMapCenter() {
    }

    @Override
    protected void onResetMapCenter() {
    }

    

    public IBaseScreenMapView getMapView() {
        return mBinding.clusterMapview;
    }

    /*private void showFloatingWindow() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M && !Settings.canDrawOverlays(this)) {
            val intent = Intent(
                    Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
                    Uri.parse("package:$packageName")
            )
            startActivityForResult(intent, REQUEST_CODE_DRAW_OVERLAY_PERMISSION)
        } else {
            startFloatingWindowService()
        }
    }*/
}

