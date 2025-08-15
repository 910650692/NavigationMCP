package com.sgm.navi.hmi.cluster.cluster_map;


import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.content.res.Configuration;
import android.graphics.drawable.Drawable;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;

import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.fsa.FsaConstant;
import com.sgm.navi.fsa.MyFsaService;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.cluster.ClusterViewModel;
import com.sgm.navi.hmi.databinding.ActivityClusterBinding;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.signal.SignalCallback;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.utils.ActivityCloseManager;
import com.sgm.navi.utils.OnCloseActivityListener;

public class ClusterActivity extends BaseActivity<ActivityClusterBinding, ClusterViewModel> implements OnCloseActivityListener, SignalCallback {
    private static final String TAG = "ClusterActivityTAG";
    private static final int SYSTEM_STATE = 6;
    //挖洞
    private static final String MAP_DISPLAYING_TRUE = "{\"isMapDisplaying\":true}";
    //填洞
    private static final String MAP_DISPLAYING_FALSE = "{\"isMapDisplaying\":false}";

    private boolean isDisplayCluster = false;// 是否上屏

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
    public void onInitView(){
        Logger.d(TAG, "onInitView");
        ActivityCloseManager.getInstance().addOnCloseListener(this);
        mViewModel.registerClusterMap();
        initViewTheme();
        SignalPackage.getInstance().registerObserver(TAG, this);
        if (SignalPackage.getInstance().getSystemState() == SYSTEM_STATE){//熄火状态
            Logger.d(TAG, "state：" , SignalPackage.getInstance().getSystemState());
            mBinding.cluster.setVisibility(GONE);
            finishAndRemoveTask();
        }
    }

    @Override
    public void onSystemStateChanged(int state) {
        if (state == SYSTEM_STATE){//熄火状态
            Logger.d(TAG, "state：" , state);
            mBinding.cluster.setVisibility(GONE);
            finishAndRemoveTask();
        }
    }

    @Override
    public void onInitData() {
        Logger.d(TAG, "onInitData");
    }


    @Override
    protected void onStart() {
        super.onStart();
        Logger.d(TAG, "----onStart");
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        overridePendingTransition(0,0);
    }

    @Override
    protected void onResume() {
        overridePendingTransition(0,0);
        super.onResume();
        Logger.d(TAG, "----onResume");
        mViewModel.remainingMileageConstraintLayoutVisibility.set(NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING));
       // mBinding.clusterMapview.postDelayed(() -> MyFsaService.getInstance().sendEvent(FsaConstant.FsaFunction.ID_SERVICE_HOLE, MAP_DISPLAYING_TRUE),500);
        if (isDisplayCluster) {
            mBinding.clusterMapview.postDelayed(() -> MyFsaService.getInstance()
                    .sendEvent(FsaConstant.FsaFunction.ID_SERVICE_HOLE, MAP_DISPLAYING_TRUE),100);
        }
    }

    @Override
    protected void onPause() {
        overridePendingTransition(0,0);
        super.onPause();
        Logger.d(TAG, "----onPause");
    }

    @Override
    protected void onStop() {
        super.onStop();
        Logger.d(TAG, "----onStop");
    }

    @Override
    public void onWindowFocusChanged(boolean z) {
        super.onWindowFocusChanged(z);
        Logger.d(TAG, "----onWindowFocusChanged: " + z);
    }


    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "----onDestroy");
        ActivityCloseManager.getInstance().removeOnCloseListener(this);
        SignalPackage.getInstance().unregisterObserver(TAG);
        ThreadManager.getInstance().removeHandleTask(holeAction);
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        Logger.d(TAG, "onConfigurationChanged");
        initViewTheme();
        ThreadManager.getInstance().runAsync(this::updateMapThemeType);
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

    public void setDayShowHide(boolean isShow){
        mBinding.stvArrivalDay.setVisibility(isShow ? VISIBLE : GONE);
    }

    public void initViewTheme(){
        int cluster_unit_transparent_sixty = ContextCompat.getColor(this, com.sgm.navi.ui.R.color.cluster_unit_transparent_sixty);
        int cluster_title_transparent_ninety = ContextCompat.getColor(this, com.sgm.navi.ui.R.color.cluster_title_transparent_ninety);
        Drawable cluster_title_view_bg = ContextCompat.getDrawable(this, R.drawable.cluster_title_view_bg);
        mBinding.stvArriveTime.setTextColor(cluster_title_transparent_ninety);
        mBinding.stvArrivalDay.setTextColor(cluster_title_transparent_ninety);
        mBinding.stvArriveDefault.setTextColor(cluster_unit_transparent_sixty);
        mBinding.remainingMileage.setTextColor(cluster_title_transparent_ninety);
        mBinding.remainingMileageUtil.setTextColor(cluster_unit_transparent_sixty);
        mBinding.remainingMileageConstraintLayout.setBackgroundDrawable(cluster_title_view_bg);
    }


    @Override
    public void onOpenOrClose(boolean isCluster, boolean isOpen) {
        if (isCluster) {
            Logger.d(TAG, "----ClusterActivity onOpenOrClose");
            isDisplayCluster = isOpen;
            if (isOpen) {
                ThreadManager.getInstance().removeHandleTask(finishAndRemoveTaskAction);
            } else {
//            mBinding.cluster.setVisibility(GONE);
                moveTaskToBack(true);
                ThreadManager.getInstance().postDelay(finishAndRemoveTaskAction, 2000);
            }
        }
    }

    /**
     * 绑定地图
     */
    public void bindMapView() {
        Logger.d(TAG, "bindMapView");
        MapPackage.getInstance().bindMapView(mBinding.clusterMapview);
        ThreadManager.getInstance().postDelay(holeAction,600);
    }

    private final Runnable holeAction = () -> {
        Logger.d(TAG, "holeAction:post");
        MyFsaService.getInstance().sendEvent(FsaConstant.FsaFunction.ID_SERVICE_HOLE, MAP_DISPLAYING_TRUE);
    };

    private final Runnable finishAndRemoveTaskAction = () -> {
        Logger.d(TAG, "finishAndRemoveTask:post");
        finishAndRemoveTask();
    };
}