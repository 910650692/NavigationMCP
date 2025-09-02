package com.sgm.navi.hmi.rearscreen;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.content.res.Configuration;

import androidx.annotation.NonNull;

import com.android.utils.ScreenUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ActivityRearScreenBinding;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.signal.SignalCallback;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.utils.ActivityCloseManager;
import com.sgm.navi.utils.OnOpenOrCloseActivityListener;

/**
 * 557车型:后排吸顶屏
 */
public class RearScreenActivity extends BaseActivity<ActivityRearScreenBinding, RearScreenViewModel> implements OnOpenOrCloseActivityListener, SignalCallback {

    private static final String TAG = RearScreenActivity.class.getSimpleName();
    private static final int SYSTEM_STATE_ENGINE_OFF = 6;//熄火状态

    private int currentUiMode;

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.REAR_SCREEN_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_rear_screen;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(TAG, "onInitView");
        mBinding.setViewModel(mViewModel);
        currentUiMode = getResources().getConfiguration().uiMode;
        ActivityCloseManager.getInstance().addOnOpenOrCloseListener(this);
        mViewModel.registerRearScreenMap();
        SignalPackage.getInstance().registerObserver(TAG, this);
        if (SignalPackage.getInstance().getSystemState() == SYSTEM_STATE_ENGINE_OFF) {//熄火状态
            Logger.d(TAG, "state:" , SignalPackage.getInstance().getSystemState());
            mBinding.sclRearScreen.setVisibility(GONE);
            finishAndRemoveTask();
        }
    }

    @Override
    public void onSystemStateChanged(int state) {
        if (state == SYSTEM_STATE_ENGINE_OFF) {//熄火状态
            Logger.d(TAG, "state: " , state);
            mBinding.sclRearScreen.setVisibility(GONE);
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
    protected void onResume() {
        super.onResume();
        Logger.d(TAG, "----onResume");
    }

    @Override
    protected void onPause() {
        super.onPause();
        Logger.d(TAG, "----onPause");
    }

    @Override
    protected void onStop() {
        super.onStop();
        Logger.d(TAG, "----onStop");
    }

    @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        super.onWindowFocusChanged(hasFocus);
        Logger.d(TAG, "----onWindowFocusChanged: " + hasFocus);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "----onDestroy");
        ActivityCloseManager.getInstance().removeOnOpenOrCloseListener(this);
        SignalPackage.getInstance().unregisterObserver(TAG);
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        Logger.d(TAG, "onConfigurationChanged");
        final int tmpUiMode = newConfig.uiMode;
        if (currentUiMode != tmpUiMode) {
            updateMapThemeType();
            recreate();
            Logger.i(TAG, "主题发生变化，重置UI!");
        }
        currentUiMode = tmpUiMode;
    }

    private void updateMapThemeType() {
        boolean nightModeEnabled = ThemeUtils.INSTANCE.isNightModeEnabled(this);
        Logger.d(TAG, "updateMapThemeType:nightModeEnabled:" + nightModeEnabled);
        ThemeType colorMode = nightModeEnabled ? ThemeType.NIGHT : ThemeType.DAY;
        MapAdapter.getInstance().updateUiStyle(MapType.REAR_SCREEN_MAP, colorMode);
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.rearScreenMapView;
    }

    public void setDayShowOrHide(boolean isShow) {
        mBinding.stvArrivalDay.setVisibility(isShow ? VISIBLE : GONE);
    }

    @Override
    public void onOpenOrClose(int mapType, boolean isOpen) {
        if (mapType == MapType.REAR_SCREEN_MAP.getMapType() && !isOpen) {
            Logger.d(TAG, "----RearScreenActivity onClose");
            mBinding.sclRearScreen.setVisibility(GONE);
            finishAndRemoveTask();
        }
    }

    /**
     * 绑定地图
     */
    public void bindMapView() {
        Logger.d(TAG, "bindMapView");
        MapPackage.getInstance().bindMapView(mBinding.rearScreenMapView);
    }

    public void updatePreviewStatus(boolean isPreviewStatus) {
        if (isPreviewStatus) {
            mBinding.sivNaviOverviewSwitch.setBackgroundResource(com.sgm.navi.scene.R.drawable.img_look_through_black_rear_screen);
        } else {
            mBinding.sivNaviOverviewSwitch.setBackgroundResource(com.sgm.navi.scene.R.drawable.img_look_through_black_58);
        }
    }

    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        mBinding.sceneNaviTmc.onNaviInfo(naviETAInfo);
    }

    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, boolean isShowAutoAdd) {
        mBinding.sceneNaviTmc.setIsShowAutoAdd(isShowAutoAdd);
        mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
    }

    /***
     * 获取车标屏幕中心点
     */
    public int[] getCarSelfPosition() {
        final int screenHeight = ScreenUtils.Companion.getInstance().getRealScreenHeight(this);
        final int top = (int) getResources().getDimension(com.sgm.navi.ui.R.dimen.rear_screen_car_logo_top);
        int[] pos = new int[2];
        pos[0] = mBinding.getRoot().getWidth() / 2;
        pos[1] = top;
        Logger.d(TAG, "pos[0]:" + pos[0], "pos[1]:" + pos[1]);
        return pos;
    }

}
