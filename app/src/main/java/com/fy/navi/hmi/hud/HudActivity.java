package com.fy.navi.hmi.hud;

import android.content.res.Configuration;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ScreenUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityHudBinding;
import com.fy.navi.hmi.hud.HudViewModel;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.ui.base.BaseActivity;
import com.fy.navi.utils.ActivityCloseManager;
import com.fy.navi.utils.OnCloseActivityListener;

public class HudActivity extends BaseActivity<ActivityHudBinding, HudViewModel>{

    private static final String TAG = "HudActivityTAG";

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.HUD_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_hud;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(TAG, "onInitView");
        setHudMapViewWH();
    }

    @Override
    public void onInitData() {
        Logger.d(TAG, "onInitData");
    }

    @Override
    protected void onResume() {
        super.onResume();
        Logger.d(TAG, "onResume");
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
        MapAdapter.getInstance().updateUiStyle(MapType.HUD_MAP, colorMode);
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.hudMapview;
    }
    public void setHudMapViewWH(){
        IBaseScreenMapView hudMapView = getMapView();
        mBinding.hudMapview.setIsBindView(true);
        if (hudMapView instanceof View) {
            View mapView = (View) hudMapView;

            ScreenUtils screenUtils = ScreenUtils.Companion.getInstance();
            int widthDp = screenUtils.dp2px(328);
            int heightDp = screenUtils.dp2px(172);
            ViewGroup.LayoutParams params = mapView.getLayoutParams();
            params.width = widthDp;
            params.height = heightDp;
            mapView.setLayoutParams(params);
        }
    }
}