package com.fy.navi.hmi.hud;

import android.os.Bundle;
import android.view.MotionEvent;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.INaviInitListener;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityMapHudactivityBinding;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseActivity;

public class HUDMapActivity extends BaseActivity<ActivityMapHudactivityBinding, HUDViewModel> implements INaviInitListener {
    private static final String TAG = "MapHUDActivity";
    private boolean initHudMap;

    @Override
    public int onLayoutId() {
        Logger.d(TAG, "onLayoutId");
        return R.layout.activity_map_hudactivity;
    }
    @Override
    public void onCreateBefore() {
        Logger.d(TAG, "onCreateBefore");
        mScreenId = MapType.HUD_MAP.name();
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (NaviService.isMapInited) {
            HUDManager.getInstance().startInitService();
            NaviService.registerAppInitListener(this);
        }
    }

    @Override
    public void onInitView() {
        if (NaviService.isMapInited) {
            mViewModel.loadMapView(getMapView());
        }
    }
    @Override
    public int onInitVariableId() {
        Logger.d(TAG, "onInitVariableId");
        return BR.ViewModel;
    }
    @Override
    public void onInitData() {
        Logger.d(TAG, "onInitData");

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
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.hudMapview;
    }
    @Override
    public void onInitFinished(boolean isSuccess) {
        if (isSuccess){
            mViewModel.loadMapView(getMapView());
        }
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        return super.onTouchEvent(event);
    }
}