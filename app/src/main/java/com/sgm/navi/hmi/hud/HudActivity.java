package com.sgm.navi.hmi.hud;

import android.content.res.Configuration;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;

import com.android.utils.ScreenUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.fsa.MyFsaService;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ActivityHudBinding;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.utils.HudMapConfigUtil;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.utils.ActivityCloseManager;
import com.sgm.navi.utils.OnCloseActivityListener;

public class HudActivity extends BaseActivity<ActivityHudBinding, HudViewModel> implements OnCloseActivityListener {

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
        ActivityCloseManager.getInstance().addOnCloseListener(this);
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
        Logger.d(TAG, "isShowHud:", MyFsaService.getInstance().isShowHud);
        if (MyFsaService.getInstance().isShowHud) {
            mBinding.hudMapview.setVisibility(View.VISIBLE);
        }
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
        ActivityCloseManager.getInstance().removeOnCloseListener(this);
        Logger.d(TAG, "onDestroy");

    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        Logger.d(TAG, "onConfigurationChanged");
        // updateMapThemeType();
    }

    private void updateMapThemeType() {
        boolean nightModeEnabled = ThemeUtils.INSTANCE.isNightModeEnabled(this);
        Logger.d(TAG, "updateMapThemeType:nightModeEnabled:" , nightModeEnabled);
        ThemeType colorMode = nightModeEnabled ? ThemeType.NIGHT : ThemeType.DAY;
        MapAdapter.getInstance().updateUiStyle(MapType.HUD_MAP, colorMode);
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.hudMapview;
    }
    public void setHudMapViewWH(){
        IBaseScreenMapView hudMapView = getMapView();
        mBinding.hudMapview.setIsBindView(true);
        mBinding.hudMapview.setOpenScreen(false);
        if (hudMapView instanceof View) {
            View mapView = (View) hudMapView;
            int widthDp = HudMapConfigUtil.getInstance().getHudMapWidth();
            int heightDp = HudMapConfigUtil.getInstance().getHudMapHeight();;
            ViewGroup.LayoutParams params = mapView.getLayoutParams();
            params.width = widthDp;
            params.height = heightDp;
            mapView.setLayoutParams(params);
        }
    }

    @Override
    public void onOpenOrClose(boolean isCluster, boolean isOpen) {
        if (!isCluster && !isOpen) {
            Logger.d(TAG, "hud close");
            mBinding.hudMapview.setVisibility(View.GONE);
            //finishAndRemoveTask();
        }
    }
}