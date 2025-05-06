package com.fy.navi.hmi.hud;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityMapHudactivityBinding;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseActivity;

public class HUDMapActivity extends BaseActivity<ActivityMapHudactivityBinding, HUDViewModel> {
    private static final String TAG = "MapHUDActivity";

    @Override
    public void onCreateBefore() {
        Logger.d(TAG, "onCreateBefore");
        mScreenId = MapType.HUD_MAP.name();
    }

    @Override
    public int onLayoutId() {
        Logger.d(TAG, "onLayoutId");
        return R.layout.activity_map_hudactivity;
    }

    @Override
    public void onInitView() {

    }

    @Override
    public void onInitData() {

    }

    @Override
    public int onInitVariableId() {
        Logger.d(TAG, "onInitVariableId");
        return BR.ViewModel;
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
}