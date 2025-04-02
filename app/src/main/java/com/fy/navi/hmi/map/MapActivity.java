package com.fy.navi.hmi.map;

import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.view.View;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.NaviService;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityMapBinding;
import com.fy.navi.hmi.test.TestWindow;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.route.RouteLightBarItem;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseActivity;

import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/01
 */
public class MapActivity extends BaseActivity<ActivityMapBinding, MapViewModel> {
    private static final String TAG = "MapActivity";
    private long lastClickTime = 0;
    private int testClickNum = 0;
    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_OPEN)
    public void onCreateBefore() {
        mScreenId = MapType.MAIN_SCREEN_MAIN_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_map;
    }

    @Override
    public int onFragmentId() {
        return R.id.layout_fragment;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mViewModel.loadMapView(mBinding.mainMapview);
        // 给限行设置点击事件
        mBinding.includeLimit.setViewModel(mViewModel);
        mBinding.cruiseLayout.setViewModel(mViewModel);
        final int powerType = mViewModel.powerType();
        // 油车
        if (powerType == 0) {
            mBinding.skIvBasicRouting.setImageResource(R.drawable.img_home_gas_station);
        } else {
            mBinding.skIvBasicRouting.setImageResource(R.drawable.img_basic_ic_gas_charging);
        }
        mBinding.includeMessageCenter.setViewModel(mViewModel);
        initTestWindow();
        mViewModel.startListenMsg();
        mViewModel.offlineMap15Day();
        mViewModel.offlineMap45Day();
    }

    @Override
    public void onInitData() {
        Intent intent = getIntent();
        getIntentExtra(intent);
    }

    @Override
    protected void onMoveMapCenter() {
        Logger.i(TAG, "onMoveMapCenter");
        mBinding.searchMainTab.setVisibility(View.GONE);
        mViewModel.setMapCenterInScreen(mBinding.layoutFragment.getLeft());
    }

    @Override
    protected void onMoveMapCenter(final Bundle bundle) {
        Logger.i(TAG, "onMoveMapCenter with bundle");
        mBinding.searchMainTab.setVisibility(View.GONE);
        mViewModel.setMapCenterInScreen(mBinding.layoutFragment.getLeft(), bundle);
    }

    @Override
    protected void onResetMapCenter() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.searchMainTab.setVisibility(View.VISIBLE);
            mViewModel.resetMapCenterInScreen();
        });
    }

    private void initTestWindow() {
        mBinding.testButton.setOnClickListener(v -> {
            long now = System.currentTimeMillis();
            if (now - lastClickTime < 500) {
                lastClickTime = now;
                if (testClickNum >= 6) {
                    testClickNum = 0;
                } else {
                    testClickNum++;
                    return;
                }
            } else {
                lastClickTime = now;
                return;
            }
            TestWindow.getInstance().show(this);
        });
    }

    // 更新当前的比例尺数值
    public void updateOnMapScaleChanged(String scale) {
        mBinding.sceneScaleView.updateOnMapLevelChanged(scale);
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        mViewModel.updateUiStyle(
                MapType.MAIN_SCREEN_MAIN_MAP,
                newConfig.uiMode
        );
    }

    @Override
    protected void onNewIntent(@NonNull Intent intent) {
        super.onNewIntent(intent);
        getIntentExtra(intent);
    }

    @Override
    protected void onResume() {
        super.onResume();
        mViewModel.getCurrentCityLimit();
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_HIDE)
    protected void onStop() {
        Logger.i(TAG, "onStop");
        super.onStop();
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_CLOSE)
    protected void onDestroy() {
        // 退出的时候主动保存一下最后的定位信息
        mViewModel.saveLastLocationInfo();
        Logger.i(TAG, "onDestroy");
        NaviService.exitProcess();
        super.onDestroy();
    }

    private void getIntentExtra(Intent intent) {
        if (null != intent) {
            //外部应用打开地图时指定的响应界面
            int intentPage = intent.getIntExtra(INaviConstant.PAGE_EXTRA, INaviConstant.OpenIntentPage.NONE);
            Logger.i(TAG, "intentPage:" + intentPage);
            intent.putExtra(INaviConstant.PAGE_EXTRA, INaviConstant.OpenIntentPage.NONE);
            mViewModel.stopCruise();
            switch (intentPage) {
                case INaviConstant.OpenIntentPage.SEARCH_PAGE:
                    String keyword = intent.getStringExtra(INaviConstant.SEARCH_KEYWORD_EXTRA);
                    if (!ConvertUtils.isEmpty(keyword)) {
                        mViewModel.setExtraKeyword(keyword);
                    } else {
                        mBinding.skIvBasicSearch.callOnClick();
                    }
                    break;
                case INaviConstant.OpenIntentPage.GO_HOME:
                    mBinding.skIvBasicHome.callOnClick();
                    break;
                case INaviConstant.OpenIntentPage.GO_COMPANY:
                    mBinding.skIvBasicBus.callOnClick();
                    break;
                case INaviConstant.OpenIntentPage.POI_DETAIL_PAGE:
                    PoiInfoEntity poiInfo = intent.getParcelableExtra(INaviConstant.POI_INFO_EXTRA);
                    if (null != poiInfo) {
                        mViewModel.toPoiDetailFragment(poiInfo);
                    }
                    break;
                case INaviConstant.OpenIntentPage.ROUTE_PAGE:
                    PoiInfoEntity endPoint = intent.getParcelableExtra(INaviConstant.ROUTE_END_POI);
                    mViewModel.openRoute(endPoint);
                    break;
                case INaviConstant.OpenIntentPage.START_NAVIGATION:
                    mViewModel.startNaviForRouteOver();
                    break;
                default:
                    break;
            }
        }
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.mainMapview;
    }

    public void updateCruiseLanInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        mBinding.cruiseLayout.cruiseLanesView.onLaneInfo(isShowLane, laneInfoEntity);
    }

    public void cruiseMuteOrUnMute(boolean isOpen) {
        mBinding.cruiseLayout.ivVoice.setSelected(isOpen);
    }

    public void setTMCView(int key, List<RouteLightBarItem> routeLightBarItems) {
        ThreadManager.getInstance().postUi(() -> {
            if (key == 0) {
                mBinding.skIvBasicHomeProgress.refreshTMC(routeLightBarItems);
            } else {
                mBinding.skIvBasicBusProgress.refreshTMC(routeLightBarItems);
            }
        });
    }
}