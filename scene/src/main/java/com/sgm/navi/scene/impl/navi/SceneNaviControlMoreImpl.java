package com.sgm.navi.scene.impl.navi;

import static com.sgm.navi.service.MapDefaultFinalTag.MAP_TOUCH;

import android.annotation.SuppressLint;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.api.navi.ISceneNaviControl;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.ui.navi.SceneNaviControlMoreView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.view.SwipeView;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RoutePriorityType;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RouteWayID;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.speech.SpeechPackage;

import java.util.HashMap;
import java.util.concurrent.ScheduledFuture;

public class SceneNaviControlMoreImpl extends BaseSceneModel<SceneNaviControlMoreView> implements
        ISceneNaviControl, SwipeView.DownSwipeAndClickListener {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CONTROL_MORE_IMPL;
    private final NaviPackage mNaviPackage;
    private MapPackage mMapPackage;
    private RoutePackage mRoutePackage;
    private SettingPackage mSettingPackage;
    private ImmersiveStatusScene mImmersiveStatusScene;
    private long mLastClickTime;
    private int mVehicleType;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;
    public SceneNaviControlMoreImpl(final SceneNaviControlMoreView screenView) {
        super(screenView);
        mNaviPackage = NaviPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mImmersiveStatusScene = ImmersiveStatusScene.getInstance();
        mLastClickTime = System.currentTimeMillis();
    }

    @SuppressLint("WrongConstant")
    @Override
    protected void onCreate() {
        super.onCreate();
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
        if (mCallBack != null) {
            HashMap<NaviSceneId, Integer> map = mCallBack.getSceneStatus();
            if (!ConvertUtils.isEmpty(map)) {
                if (map.containsKey(NaviSceneId.NAVI_SCENE_CONTROL_MORE)) {
                    Integer status = map.get(NaviSceneId.NAVI_SCENE_CONTROL_MORE);
                    Logger.i(TAG, "SceneNaviControlMoreImpl onCreate", " visible = ",
                            status);
                    if (status != null && status == NumberUtils.NUM_1) {
                        initTimer();
                    }
                }
            }
        }
        if (mScreenView != null) {
            mScreenView.setSwipeListener(this);
        }
    }

    @Override
    public void closeNavi() {
    }

    @Override
    public void naviContinue() {
    }

    @Override
    public void moreSetup() {
    }

    @Override
    public void backControl(){
        setImmersiveStatus(ImersiveStatus.TOUCH);
        if(mCallBack != null){
            mCallBack.skipNaviControlScene();
        }
    }

    @Override
    public void switchOverview() {
    }

    @Override
    public void onVariation() {
    }


    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_REFRESHMAP)
    public void refreshRoute() {
        Logger.i(TAG, "refreshRoute");
        long currentTime = System.currentTimeMillis();
        boolean isCanRefreshRoute = currentTime - mLastClickTime > NumberUtils.NUM_2000;
        if (!isCanRefreshRoute) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.current_newest_path));
            return;
        }
        mLastClickTime = currentTime;
        requestReRoute();
        notifySceneStateChange(false);
        if (mCallBack != null) {
            mCallBack.skipNaviControlScene();
        }
    }

    /**
     * 网络状态导致的刷新
     */
    public void refreshRouteCauseNet() {
        boolean isCanRefresh = TimerHelper.isCanRefreshRoute();
        if (!isCanRefresh) {
            boolean currentNetStatus = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
            ThreadManager.getInstance().postDelay(new Runnable() {
                @Override
                public void run() {
                    boolean netStatus = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
                    if (currentNetStatus != netStatus) {
                        requestReRoute();
                    }
                }
            }, NumberUtils.NUM_3000);
            return;
        }
        requestReRoute();
    }

    /**
     * 请求路线刷新
     */
    private void requestReRoute() {
        ThreadManager.getInstance().execute(() -> {
            final RouteRequestParam param = new RouteRequestParam();
            param.setMMapTypeId(mMapTypeId);
            param.setMRouteWay(RouteWayID.ROUTE_WAY_REFRESH);
            param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH);
            // 算路那边在线刷新失败后会自动调用离线刷新，所以这边就调用一个接口就好
            mRoutePackage.requestRoute(param);
        });
    }

    @Override
    public void naviBroadcast() {
        Logger.i(TAG, "naviBroadcast");
        setImmersiveStatus(ImersiveStatus.TOUCH);
        switchBroadcastMode();
    }

    /*** 切换播报模式***/
    public void switchBroadcastMode() {
        int broadcastMode = mSettingPackage.getConfigKeyBroadcastMode();
        broadcastMode = switch (broadcastMode) {
            case NaviConstant.BroadcastType.BROADCAST_DETAIL -> NaviConstant.BroadcastType.BROADCAST_CONCISE;
            case NaviConstant.BroadcastType.BROADCAST_CONCISE -> NaviConstant.BroadcastType.BROADCAST_MINIMALISM;
            default -> NaviConstant.BroadcastType.BROADCAST_DETAIL;
        };
        mSettingPackage.setConfigKeyBroadcastMode(broadcastMode);
        broadcastModeSwitchTts(broadcastMode);
        mScreenView.updateBroadcast(broadcastMode);
        Logger.i(TAG, "updateBroadcast：", broadcastMode);

        //For Bury Point
        sendBroadcastModeTts(broadcastMode);
    }

    public void switchBroadcastMode(int broadcastMode) {
        mSettingPackage.setConfigKeyBroadcastMode(broadcastMode);
        mScreenView.updateBroadcast(broadcastMode);
        Logger.i(TAG, "updateBroadcast：", broadcastMode);

        //For Bury Point
        sendBroadcastModeTts(broadcastMode);
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_VOICE_SELECT)
    private void sendBroadcastModeTts(int broadcastMode) {
        String tts = switch (broadcastMode) {
            case NaviConstant.BroadcastType.BROADCAST_CONCISE -> BuryConstant.BroadcastMode.CONCISE;
            case NaviConstant.BroadcastType.BROADCAST_MINIMALISM -> BuryConstant.BroadcastMode.MINIMALISM;
            default -> BuryConstant.BroadcastMode.DETAIL;
        };
        BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, tts)
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    private void broadcastModeSwitchTts(int broadcastMode) {
        String tts = switch (broadcastMode) {
            case NaviConstant.BroadcastType.BROADCAST_CONCISE ->
                    String.format(ResourceUtils.Companion.getInstance().
                                    getString(R.string.navi_broadcast_switch), ResourceUtils.Companion.getInstance().getString(R.string.navi_broadcast_concise));
            case NaviConstant.BroadcastType.BROADCAST_MINIMALISM ->
                    String.format(ResourceUtils.Companion.getInstance().getString(R.string.navi_broadcast_switch),
                            ResourceUtils.Companion.getInstance(). getString(R.string.navi_broadcast_minimalism));
            default -> String.format(ResourceUtils.Companion.getInstance().getString(R.string.navi_broadcast_switch),
                    ResourceUtils.Companion.getInstance().getString(R.string.navi_broadcast_detail));
        };
        SpeechPackage.getInstance().synthesizeLast(tts);
    }

    @Override
    public void routePreference() {
        Logger.i(TAG, "routePreference");
        setImmersiveStatus(ImersiveStatus.TOUCH);
        if (mCallBack != null) {
            mCallBack.skipNaviPreferenceScene();
        }
    }

    @Override
    public void carHead() {
        Logger.i(TAG, "carHead");
        final boolean isFixedOverView = NaviPackage.getInstance().getFixedOverViewStatus();
        final boolean isClusterOverView = NaviPackage.getInstance().getClusterFixOverViewStatus();
        if (isFixedOverView || isClusterOverView) {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(
                            R.string.navi_car_head_switch_while_overview));
            return;
        }
        setImmersiveStatus(ImersiveStatus.TOUCH);
        MapMode currentMapMode = mMapPackage.getCurrentMapMode(mMapTypeId);
        boolean result = mMapPackage.switchMapMode(mMapTypeId);
        MapMode switchedMapMode = mMapPackage.getCurrentMapMode(mMapTypeId);
        String modeText = mScreenView.updateCarModel(switchedMapMode);
        //如果切换前后模式一样，没有切换成功发toast提示
        if (!result || currentMapMode == switchedMapMode) {
            ToastUtils.Companion.getInstance().showCustomToastView(String.
                    format(ResourceUtils.Companion.getInstance().getString(R.string.navi_map_mode_switch_fail), modeText));
        }
    }

    @Override
    public void naviSetting() {
        Logger.i(TAG, "naviSetting");
        if (mCallBack != null) {
            mCallBack.skipSettingFragment();
        }
    }

    @Override
    public void alongSearch(final int index) {
        Logger.i(TAG, "alongSearch index:", index, " mVehicleType:", mVehicleType);
        setImmersiveStatus(ImersiveStatus.TOUCH);
        OpenApiHelper.enterPreview(mMapTypeId);
        switch (index) {
            case 0:
                if (mVehicleType == 1) {//电车
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_charge), OpenApiHelper.ALONG_WAY);
                } else {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_station), OpenApiHelper.ALONG_WAY);
                }
                break;
            case 1:
                if (mVehicleType == 1 || mVehicleType == 0) {//电车-油车
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_lavatory), OpenApiHelper.ALONG_WAY);
                } else {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_charge), OpenApiHelper.ALONG_WAY);
                }
                break;
            case 2:
                if (mVehicleType == 1 || mVehicleType == 0) {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_parking), OpenApiHelper.ALONG_WAY);
                } else {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_lavatory), OpenApiHelper.ALONG_WAY);
                }
                break;
            case 3:
                if (mVehicleType == 1 || mVehicleType == 0) {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.navi_along_service), OpenApiHelper.ALONG_WAY);
                } else {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_parking), OpenApiHelper.ALONG_WAY);
                }
                break;
            case 4:
                mCallBack.goAlongWayList();
                break;
            default:
                break;
        }
    }

    @Override
    public ObservableField<Boolean> getGroupMoreSetupField() {
        return null;
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        mImmersiveStatusScene = null;
        cancelTimer();
        if (mScreenView != null) {
            mScreenView.removeSwipeListener();
        }
    }

    /**
     * @param currentImersiveStatus ImmersiveStatus
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "onImmersiveStatusChange currentImersiveStatus：", currentImersiveStatus);
        if (currentImersiveStatus == ImersiveStatus.TOUCH) {
        } else {
            notifySceneStateChange(false);
        }
    }

    /**
     * 开始倒计时
     */
    public void initTimer() {
        Logger.i(TAG, "initTimer");
        cancelTimer();
        mTimes = NumberUtils.NUM_8;
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(new Runnable() {
                    @Override
                    public void run() {
                        notifySceneStateChange(false);
                        if(mCallBack != null){
                            mCallBack.skipNaviControlScene();
                        }
                    }
                });
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    /**
     * 取消倒计时
     */
    public void cancelTimer() {
        Logger.i(TAG, "cancelTimer");
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    /**
     * 显示主页面
     */
    @Override
    public void showMain() {
    }

    public void notifySceneStateChange(final boolean isVisible) {
        if (mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviControlImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChangeReset((isVisible ?
                        INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), mScreenView.getSceneId(), true);
    }

    public int getCarType() {
        mVehicleType = OpenApiHelper.powerType();
        return mVehicleType;
    }

    private void setImmersiveStatus(ImersiveStatus immersiveStatus) {
        // 固定全览状态下操作控制栏不会显示继续导航按钮,已和UE确认
        if (mNaviPackage.getFixedOverViewStatus() || mNaviPackage.getClusterFixOverViewStatus()) {
            return;
        }
        initTimer();
        if (null != mImmersiveStatusScene) {
            Logger.d(TAG, MAP_TOUCH, " immersiveStatus:" , immersiveStatus);
            mImmersiveStatusScene.setImmersiveStatus(mMapTypeId, immersiveStatus);
        }
    }

    /**
     * 获取广播模式
     */
    public int getBroadcastMode(){
        return mSettingPackage.getConfigKeyBroadcastMode();
    }

    /**
     * 获取车头朝向
     */
    public MapMode getCarModel() {
        if (mMapPackage == null) {
            mMapPackage = MapPackage.getInstance();
        }
        return mMapPackage.getCurrentMapMode(mMapTypeId);
    }

    @Override
    public void onDownSwipe() {
        backControl();
    }

    @Override
    public void onClick() {
        backControl();
    }
}
