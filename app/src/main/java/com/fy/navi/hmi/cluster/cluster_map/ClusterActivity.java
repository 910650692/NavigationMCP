package com.fy.navi.hmi.cluster.cluster_map;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.fsa.FsaConstant;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.cluster.ClusterViewModel;
import com.fy.navi.hmi.databinding.ActivityClusterBinding;
import com.fy.navi.scene.impl.navi.common.AutoUIString;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.ui.base.BaseActivity;

import java.util.Objects;
public class ClusterActivity extends BaseActivity<ActivityClusterBinding, ClusterViewModel>   {

    /**
     * TAG
     */
    private static final String TAG = "ClusterActivityTAG";
    /**
     * 通过广播关闭Activity
     */
    private BroadcastReceiver mBroadcastReceiver;
    private MapDisplayingBean mapDisplayingBean;

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
        mapDisplayingBean = new MapDisplayingBean();
    }

    @Override
    protected void onStart() {
        super.onStart();
        Logger.d(TAG, "onStart");
    }

    @Override
    protected void onResume() {
        super.onResume();
        mapDisplayingBean.setMapDisplaying(true);
        String json = GsonUtils.toJson(mapDisplayingBean);
        Logger.i(TAG, "onStart: "+json);
        MyFsaService.getInstance().sendEvent(FsaConstant.FsaFunction.ID_SERVICE_HOLE,json);
        setVS(NaviStatusPackage.getInstance().getCurrentNaviStatus());
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
    protected void onStop() {
        super.onStop();
        mapDisplayingBean.setMapDisplaying(false);
        String json = GsonUtils.toJson(mapDisplayingBean);
        Logger.i(TAG, "onStop: "+json);
        MyFsaService.getInstance().sendEvent(FsaConstant.FsaFunction.ID_SERVICE_HOLE,json);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        unregisterReceiver(mBroadcastReceiver);
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

    //  到达时间
    private String mArriveTime = "";
    // 到达天数
    private String mArriveDay = "";
    //  剩余信息
    private String mRemainInfo = "";

    //  上一次显示的到达时间
    private String mLastArriveTime = "";
    //  上一次显示的剩余信息
    private String mLastRemainInfo = "";
    /**
     * 更新ETA信息
     *
     * @param distance
     * @param time
     */
    public void updateEta(final int distance, final int time) {
        if (distance <= 0 && time <= 0) {
            return;
        }
        //天数
        mArriveDay = TimeUtils.getArriveDay(time);
        //时间
        mArriveTime = TimeUtils.getArriveTime(this, time);
        //剩余距离
        mRemainInfo = TimeUtils.getRemainingMileage(this, distance);
        // 到达或者剩余信息有变化才更新界面
        if (!Objects.equals(mLastArriveTime, mArriveTime) || !Objects.equals(mLastRemainInfo, mRemainInfo)) {
            showArriveInfo();
        }
    }
    public void updateRouteName(String mCurrRouteName) {
        mBinding.stvNaviRouteName.setText(mCurrRouteName);
    }

    private void showArriveInfo() {
        Logger.i(TAG, " shwoArriveInfo ");
        // 到达时间
        if (!TextUtils.isEmpty(mArriveTime)) {
            setTextStvArriveTime(new AutoUIString(ConvertUtils.digitToBold(mArriveTime)));
        }
        setTextStvArrivalDay(new AutoUIString(mArriveDay));
        setTextremainingMileage(new AutoUIString(ConvertUtils.digitToBold(mRemainInfo)));
        mLastArriveTime = mArriveTime;
        mLastRemainInfo = mRemainInfo;
    }

    /**
     * 设置剩余距离
     */
    public void setTextremainingMileage(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaRouteRemainDefault：" + textContent.getString(this));
        mBinding.remainingMileage.setText(textContent.getString(this));
    }

    /**
     * 设置到达时间
     * @param textContent content
     */
    @SuppressLint("SetTextI18n")
    public void setTextStvArriveTime(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaRouteArrivalDefault：" +
                textContent.getString(this));
        mBinding.stvArriveTime.setText(textContent.getString(this));
    }

    /**
     * 设置到达天数
     * @param textContent context
     */
    public void setTextStvArrivalDay(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaArrivalDay：" + textContent.getString(this));
        mBinding.stvArrivalDay.setText(textContent.getString(this));
    }

    public void setVS(String naviStatus) {
        if (naviStatus.equals(NaviStatus.NaviStatusType.NAVING)){
            mBinding.routeNameConstraintLayout.setVisibility(View.VISIBLE);
            mBinding.remainingMileageConstraintLayout.setVisibility(View.VISIBLE);
        }else {
            mBinding.routeNameConstraintLayout.setVisibility(View.GONE);
            mBinding.remainingMileageConstraintLayout.setVisibility(View.GONE);
        }
    }
}

