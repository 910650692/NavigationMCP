package com.fy.navi.hmi.cluster.cluster_map;

import android.content.BroadcastReceiver;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.cluster.ClusterViewModel;
import com.fy.navi.hmi.databinding.ActivityClusterBinding;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.ui.base.BaseActivity;
import com.fy.navi.utils.ActivityCloseManager;

import java.util.Objects;

public class ClusterActivity extends BaseActivity<ActivityClusterBinding, ClusterViewModel> {

    private static final String TAG = "ClusterActivityTAG";
    private BroadcastReceiver mBroadcastReceiver;

    // ETA 相关字段
    private String mArriveTime = "";      // 到达时间
    private String mArriveDay = "";       // 到达天数
    private String mRemainInfo = "";      // 剩余距离
    private String mLastArriveTime = "";  // 上一次到达时间
    private String mLastRemainInfo = "";  // 上一次剩余距离

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
        registerCloseListener();
    }

    @Override
    protected void onStart() {
        super.onStart();
        Logger.d(TAG, "onStart");
    }

    @Override
    protected void onResume() {
        super.onResume();
        Logger.d(TAG, "onResume");
        setVS(NaviStatusPackage.getInstance().getCurrentNaviStatus());
        if (NavistatusAdapter.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)) {
            showClusterMapToast();
        }
    }

    @Override
    public int onInitVariableId() {
        Logger.d(TAG, "onInitVariableId");
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(TAG, "onInitView");
        mViewModel.loadMapView();
        updateMapThemeType();
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
        if (NavistatusAdapter.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)) {
            //OpenApiHelper.exitPreview(MapType.MAIN_SCREEN_MAIN_MAP);
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        ActivityCloseManager.getInstance().removeListener();
    }

    @Override
    public void onInitData() {
        Logger.d(TAG, "onInitData");
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        Logger.d(TAG, "onConfigurationChanged");
        updateMapThemeType();
    }

    private void registerCloseListener() {
        ActivityCloseManager.getInstance().setOnCloseListener(this::finish);
    }

    private void showClusterMapToast() {
        ThreadManager.getInstance().postUi(() ->
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(com.fy.navi.fsa.R.string.open_cluster_map_toast),
                        3000));
    }

    private void updateMapThemeType() {
        boolean nightModeEnabled = ThemeUtils.INSTANCE.isNightModeEnabled(this);
        Logger.d(TAG, "updateMapThemeType:nightModeEnabled:" + nightModeEnabled);
        ThemeType colorMode = nightModeEnabled ? ThemeType.NIGHT : ThemeType.DAY;
        MapAdapter.getInstance().updateUiStyle(MapType.CLUSTER_MAP, colorMode);
    }

    public void updateEta(final int distance, final int time) {
        if (distance <= 0 && time <= 0) return;
        mArriveDay = TimeUtils.getArriveDay(time);
        mArriveTime = TimeUtils.getArriveTime(this, time);
        mRemainInfo = TimeUtils.getRemainingMileage(this, distance);
        if (!Objects.equals(mLastArriveTime, mArriveTime) || !Objects.equals(mLastRemainInfo, mRemainInfo)) {
            showArriveInfo();
        }
    }

    private void showArriveInfo() {
        Logger.i(TAG, "showArriveInfo");
        if (!TextUtils.isEmpty(mArriveTime)) {
            mBinding.stvArriveTime.setText(ConvertUtils.digitToBold(mArriveTime));
        }
        mBinding.stvArrivalDay.setText(mArriveDay);
        mBinding.remainingMileage.setText(ConvertUtils.digitToBold(mRemainInfo));
        mLastArriveTime = mArriveTime;
        mLastRemainInfo = mRemainInfo;
    }

    public void updateRouteName(String routeName) {
        mBinding.stvNaviRouteName.setText(routeName);
    }

    // ========= 导航状态控制 =========
    public void setVS(String naviStatus) {
        boolean isNavigating = NaviStatus.NaviStatusType.NAVING.equals(naviStatus);
        mBinding.routeNameConstraintLayout.setVisibility(isNavigating ? View.VISIBLE : View.GONE);
        mBinding.remainingMileageConstraintLayout.setVisibility(isNavigating ? View.VISIBLE : View.GONE);
    }

    public IBaseScreenMapView getMapView() {
        Logger.d(TAG, "getMapView");
        return mBinding.clusterMapview;
    }
}