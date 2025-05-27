package com.fy.navi.hmi.navi;


import android.annotation.SuppressLint;
import android.graphics.Path;
import android.graphics.drawable.BitmapDrawable;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;

import androidx.fragment.app.Fragment;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.launcher.FloatViewManager;
import com.fy.navi.hmi.search.alongway.MainAlongWaySearchFragment;
import com.fy.navi.hmi.search.searchresult.SearchResultFragment;
import com.fy.navi.hmi.setting.SettingFragment;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.layer.refix.DynamicLevelMode;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.message.MessageCenterInfo;
import com.fy.navi.service.define.message.MessageCenterType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.NextManeuverEntity;
import com.fy.navi.service.define.navi.PathsTrafficEventInfoEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navi.SuggestChangePathReasonEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.route.RouteWeatherInfo;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.message.MessageCenterManager;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;
import com.fy.navi.ui.base.StackManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class NaviGuidanceModel extends BaseModel<NaviGuidanceViewModel> implements
        IGuidanceObserver, ImmersiveStatusScene.IImmersiveStatusCallBack, ISceneCallback,
        IRouteResultObserver, NetWorkUtils.NetworkObserver, ILayerPackageCallBack,
        SearchResultCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private final RoutePackage mRoutePackage;
    private final LayerPackage mLayerPackage;
    private final MapPackage mMapPackage;
    private final MessageCenterManager messageCenterManager;
    private ImersiveStatus mCurrentStatus = ImersiveStatus.IMERSIVE;
    private List<NaviViaEntity> mViaList = new ArrayList<>();
    private NaviEtaInfo mNaviEtaInfo;

    private Handler mHandler;
    private Runnable mRunnable;
    private int mEndSearchId;

    public static final int BRIDGE_SWITCH = 1;
    public static final int ROAD_SWITCH = 0;
    public static final String MAIN_ROAD = "MAIN";
    public static final String SIDE_ROAD = "SIDE";
    public static final String BRIDGE_ON = "ON";
    public static final String BRIDGE_UNDER = "UNDER";
    public static final int ENTER_PREVIEW = 1;
    public static final int EXIT_PREVIEW = 0;
    private NetWorkUtils mNetWorkUtils;
    private Boolean mCurrentNetStatus;
    private NaviGuidanceHelp mModelHelp;
    private boolean mIsShowAutoAdd = true; // 是否显示自动添加的充电桩

    public static final int ONE_SECOND = 1000;
    private List<OnNetStatusChangeListener> mNetStatusChangeListeners =
            new CopyOnWriteArrayList<>();

    private final SearchPackage mSearchPackage;
    private ChargeTipManager mTipManager;
    private String mFilename = "";
    // 路口大图是否显示
    private boolean mIsShowCrossImage;
    // 记录路口大图出现时的起始距离
    private int mMoveStartDistance;
    private NextManeuverEntity mNextManeuverEntity;

    public NaviGuidanceModel() {
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mNetWorkUtils = NetWorkUtils.Companion.getInstance();
        messageCenterManager = MessageCenterManager.getInstance();
        mModelHelp = new NaviGuidanceHelp();
    }

    /**
     * 终点搜，为了引导的终点扎标，显示营业时间/充电站信息
     */
    private void endPoiSearch() {
        RouteParam endRouteParam = mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
        mEndSearchId = mSearchPackage.poiIdSearch(endRouteParam.getPoiID(), true);
        Logger.i(TAG, "mEndSearchId = " + mEndSearchId);
    }

    public boolean getIsShowAutoAdd() {
        return mIsShowAutoAdd;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        NaviSceneManager.getInstance().onCreateSceneView();
        ImmersiveStatusScene.getInstance().registerCallback("NaviGuidanceModel", this);
        mNaviPackage.registerObserver(NaviConstant.KEY_NAVI_MODEL, this);
        mRoutePackage.registerRouteObserver(NaviConstant.KEY_NAVI_MODEL, this);
        mNetWorkUtils.registerNetworkObserver(this);
        mCurrentNetStatus = mNetWorkUtils.checkNetwork();
        mLayerPackage.registerCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mSearchPackage.registerCallBack(NaviConstant.KEY_NAVI_MODEL, this);
        mHandler = new Handler(Looper.getMainLooper());
        mRunnable = new Runnable() {
            @Override
            public void run() {
                endPoiSearch();
                mHandler.removeCallbacks(mRunnable);
                mHandler.postDelayed(this,
                        NumberUtils.NUM_3 * NumberUtils.NUM_60 * NumberUtils.NUM_1000);
            }
        };
        // 因为生命周期是和HMI绑定的，如果页面重启并且是在导航台进入三分钟一次的终点POI查询
        if (NaviStatus.NaviStatusType.NAVING.equals(
                NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            mHandler.removeCallbacks(mRunnable);
            mHandler.post(mRunnable);
        }
        mNextManeuverEntity = new NextManeuverEntity();
    }

    @Override
    public void onStart() {
        super.onStart();
    }

    /**
     * 开始导航
     *
     * @param bundle bundle
     */
    public void startNavigation(final Bundle bundle) {
        final boolean isNaviSuccess;
        if (bundle != null) {
            final int anInt = bundle.getInt(
                    AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM,
                    AutoMapConstant.NaviType.NAVI_GPS);
            final boolean isSimulate = anInt == AutoMapConstant.NaviType.NAVI_SIMULATE;
            isNaviSuccess = mNaviPackage.startNavigation(isSimulate);
            mViewModel.setNaviSimState(isSimulate);
        } else {
            isNaviSuccess = mNaviPackage.startNavigation(false);
        }
        if (isNaviSuccess) {
            final boolean isAutoScale = SettingPackage.getInstance().getAutoScale();
            if (isAutoScale) {
                mLayerPackage.openDynamicLevel(MapType.MAIN_SCREEN_MAIN_MAP,
                        DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
            }
            drawEndPoint(mRoutePackage.getEndEntity(MapType.MAIN_SCREEN_MAIN_MAP));
            // 开始三分钟查询一次终点POI信息
            if (null != mHandler) {
                mHandler.removeCallbacks(mRunnable);
                mHandler.post(mRunnable);
            }
            Logger.i(TAG, "startNaviSuccess");
            String isOpen = SettingPackage.getInstance().getValueFromDB(SettingController.KEY_SETTING_IS_AUTO_RECORD);
            Logger.i(TAG, "isOpen:" + isOpen);
            if (isOpen != null && isOpen.equals("true")) {
                Logger.i(TAG, "开始打点");
                @SuppressLint("HardwareIds") final String androidId = Settings.Secure.getString(
                        AppContext.getInstance().getMContext().getContentResolver(),
                        Settings.Secure.ANDROID_ID
                );
                final long curTime = System.currentTimeMillis();
                mFilename = curTime + "_" + 1 + "_" + androidId;
                UserTrackPackage.getInstance().startGpsTrack(GBLCacheFilePath.SYNC_PATH + "/403", mFilename, 2000);
            }
            mIsShowAutoAdd = true;
            final MapType mapTypeId = MapTypeManager.getInstance().
                    getMapTypeIdByName(mViewModel.mScreenId);
            mNaviPackage.addNaviRecord(true);
            mMapPackage.goToCarPosition(mapTypeId);
            mLayerPackage.setFollowMode(mapTypeId, true);
        }
    }

    /**
     * 引导终点扎标绘制
     */
    private void drawEndPoint(PoiInfoEntity poiInfoEntity) {
        if (null != poiInfoEntity) {
            LayerItemRouteEndPoint endPoint = new LayerItemRouteEndPoint();
            endPoint.setEndPointType(LayerPointItemType.ROUTE_POINT_END_BUSINESS_HOURS);
            ChargeInfo chargeInfo = poiInfoEntity.getChargeInfoList().get(0);
            String businessTime = poiInfoEntity.getBusinessTime();
            if (!ConvertUtils.isEmpty(chargeInfo)) {
                int slowTotal = chargeInfo.getMSlowTotal();
                int slowFree = chargeInfo.getMSlowFree();
                int fastTotal = chargeInfo.getMFastTotal();
                int fastFree = chargeInfo.getMFastFree();
                String chargeInfoStr = String.format(
                        ResourceUtils.Companion.getInstance().
                                getString(R.string.navi_end_charge_info),
                        fastFree, fastTotal, slowFree, slowTotal);
                endPoint.setBusinessHours(chargeInfoStr);
                mRoutePackage.updateRouteEndPoint(MapType.MAIN_SCREEN_MAIN_MAP, endPoint);
            } else if (null != businessTime) {
                endPoint.setBusinessHours(businessTime);
                mRoutePackage.updateRouteEndPoint(MapType.MAIN_SCREEN_MAIN_MAP, endPoint);
            }
        }
    }

    @Override
    public void onAttachViewModel(final NaviGuidanceViewModel baseViewModel) {
        super.onAttachViewModel(baseViewModel);
        mViewModel.addSceneCallback(this);
        mTipManager = new ChargeTipManager(mViewModel);
    }

    @Override
    public void onNaviInfo(final NaviEtaInfo naviInfoBean) {
        if (ConvertUtils.isEmpty(naviInfoBean)) return;
        mNaviEtaInfo = naviInfoBean;
        mViewModel.onNaviInfo(naviInfoBean);
        if (mTipManager != null) {
            mTipManager.setNextViaChargeStation(naviInfoBean);
        }
        if (mIsShowCrossImage) {
            // 获得行驶过的距离
            int moveDistance = mMoveStartDistance - naviInfoBean.getAllDist();
            mViewModel.onCrossProgress(moveDistance);
        } else {
            mMoveStartDistance = naviInfoBean.getAllDist();
        }
    }

    @Override
    public void onNaviStop() {
        UserTrackPackage.getInstance().closeGpsTrack(GBLCacheFilePath.SYNC_PATH + "/403", mFilename);
        mFilename = "";
        mViewModel.onNaviStop();
        mRoutePackage.removeAllRouteInfo(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        mLayerPackage.setVisibleGuideSignalLight(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), false);
        mRoutePackage.clearRouteLine(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
    }

    @Override
    public void onNaviArrive(final long traceId, final int naviType) {
        mViewModel.onNaviArrive(traceId, naviType);
    }

    @Override
    public void onManeuverInfo(final NaviManeuverInfo info) {
        mViewModel.onManeuverInfo(info);
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        Logger.i(TAG, "onUpdateTMCLightBar naviTmcInfo = " + naviTmcInfo.toString());
        mNaviPackage.setTmcData(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo, mIsShowAutoAdd);
    }

    @Override
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        mIsShowCrossImage = isShowImage;
        if (!isShowImage) {
            mViewModel.onCrossProgress(NumberUtils.NUM_ERROR);
            // 为了显示进度条铺满的过程，延迟200ms隐藏路口大图
            ThreadManager.getInstance().postDelay(new Runnable() {
                @Override
                public void run() {
                    mViewModel.onCrossImageInfo(isShowImage, naviImageInfo);
                }
            }, NumberUtils.NUM_200);
            return;
        }
        mViewModel.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    @Override
    public void onNaviSpeedOverallInfo(final SpeedOverallEntity speedCameraInfo) {
        mViewModel.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    @Override
    public void onCurrentRoadSpeed(int speed) {
        if (null != mViewModel) {
            mViewModel.onCurrentRoadSpeed(speed);
        }
    }

    @Override
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        mViewModel.onNaviSAPAInfo(sapaInfoEntity);
    }

    @Override
    public void onUpdateViaPass(final long viaIndex) {
        List<RouteParam> allPoiParamList = OpenApiHelper.getAllPoiParamList(
                MapType.MAIN_SCREEN_MAIN_MAP);
        // 删除途经点扎标
        mNaviPackage.removeViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, viaIndex + "");
        // 经过途经点后删除途经点
        if (allPoiParamList.size() > 2) {
            PoiInfoEntity poiInfo = allPoiParamList.get(1).getMPoiInfoEntity();
            boolean isDeleteSuccess = mRoutePackage.removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                    poiInfo, false);
            // 删除后更新途经点列表信息
            if (null != mViewModel) {
                mViewModel.updateViaListImm();
            }
            Logger.i(TAG, "onUpdateViaPass isDeleteSuccess = " + isDeleteSuccess);
        }
        mViewModel.onUpdateViaPass(viaIndex);
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_NEWROUTE)
    public void onSelectMainPathStatus(final long pathID, final int result) {
        Logger.i(TAG, "onSelectMainPathStatus pathID = " + pathID + " result = " + result);
        if (result == NaviConstant.ChangeNaviPathResult.CHANGE_NAVI_PATH_RESULT_SUCCESS) {
            mNaviPackage.onlyShowCurrentPath();
        }
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        mViewModel.onLaneInfo(isShowLane, laneInfo);
    }

    @Override
    public void onRouteFail(final MapType mapTypeId, final String errorMsg) {
        Logger.i(TAG, "onRouteFail");
        mViewModel.updateViaList();
    }

    @Override
    public void onRouteResult(final RequestRouteResult requestRouteResult) {
        Logger.i(TAG, "onRouteResult");
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
    }

    @Override
    public void onDriveReport(final NaviDriveReportEntity naviDriveReportEntity) {
        Logger.i(TAG, "onDriveReport " + naviDriveReportEntity.toString());
        mViewModel.onDriveReport(naviDriveReportEntity);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        NaviSceneManager.getInstance().destroySceneView();
        mNetWorkUtils.unRegisterNetworkObserver(this);
        ImmersiveStatusScene.getInstance().unRegisterCallback("NaviGuidanceModel");
        mSearchPackage.unRegisterCallBack(NaviConstant.KEY_NAVI_MODEL);
        mLayerPackage.unRegisterCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        if (mNaviPackage != null) {
            mNaviPackage.unregisterObserver(NaviConstant.KEY_NAVI_MODEL);
        }
        if (mTipManager != null) {
            mTipManager.unInit();
        }
        mHandler.removeCallbacks(mRunnable);
        mHandler = null;
        mRunnable = null;
    }

    /**
     * 显示路段转向箭头
     *
     * @param segmentsId 路段ID
     */
    public void showRouteSegmentArrow(final long segmentsId) {
        final ArrayList<Long> data = new ArrayList<>();
        data.add(segmentsId);
        mLayerPackage.setPathArrowSegment(MapTypeManager.getInstance().
                getMapTypeIdByName(mViewModel.mScreenId), data);
        mLayerPackage.updatePathArrow(MapTypeManager.getInstance().
                getMapTypeIdByName(mViewModel.mScreenId));
    }

    @Override
    public void onImmersiveStatusChange(final MapType mapTypeId,
                                        final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "NaviGuidanceModel currentImersiveStatus：" + currentImersiveStatus +
                "，mCurrentStatus：" + mCurrentStatus);
        if (!NaviStatus.NaviStatusType.NAVING.equals(NaviStatusPackage.getInstance().
                getCurrentNaviStatus())) {
            Logger.i(TAG, "Not in navigation, return");
            return;
        }
        if (MapType.MAIN_SCREEN_MAIN_MAP.equals(mapTypeId)) {
            setImmersiveStatus(currentImersiveStatus);
        }
        if (currentImersiveStatus != mCurrentStatus ||
                currentImersiveStatus == ImersiveStatus.TOUCH) {
            mCurrentStatus = currentImersiveStatus;
            mViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }

    @Override
    public void skipAlongWayFragment() {
        Logger.i(TAG, "skipAlongWayFragment");
        addFragment(new MainAlongWaySearchFragment(), null);
    }

    @Override
    public void goCharge() {
        ISceneCallback.super.goCharge();
        // 注：点击【去充电】，调用 沿途搜充电站接口，展示沿途充电站列表
        Logger.i(TAG, "立即去充电！");
        final Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, "充电站");
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, null);
        addFragment(new SearchResultFragment(), bundle);
    }

    @Override
    public void openSupplyPlan() {
        ISceneCallback.super.openSupplyPlan();
        // TODO
        Logger.i(TAG, "开启补能规划，重新算路！");
    }

    @Override
    public void searchNewChargeStation() {
        ISceneCallback.super.searchNewChargeStation();
        Logger.i(TAG, "当前充电站已关闭或者繁忙，查找新的充电站");
        // 当前充电站已关闭或者繁忙，查找新的充电站
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, "充电站");
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, null);
        addFragment(new SearchResultFragment(), bundle);
    }

    @Override
    public void skipSettingFragment() {
        Logger.i(TAG, "skipSettingFragment");
        addFragment(new SettingFragment(), null);
    }

    @Override
    public void deleteViaPoint(final NaviViaEntity entity) {
        ISceneCallback.super.deleteViaPoint(entity);
        if (entity.getChargeInfo() != null && entity.getChargeInfo().isAutoAdd()) {
            showDeleteAllTip(entity);
        } else {
            final PoiInfoEntity poiInfo = new PoiInfoEntity();
            poiInfo.setPoint(entity.getRealPos());
            poiInfo.setPid(entity.getPid());
            final boolean result = mRoutePackage.removeVia(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), poiInfo, true);
            mViewModel.notifyDeleteViaPointResult(result, entity);
        }
    }

    @Override
    public void updateSceneVisible(final NaviSceneId sceneType, final boolean isVisible) {
        mViewModel.updateSceneVisible(sceneType, isVisible);
        if (sceneType == NaviSceneId.NAVI_SCENE_2D_CROSS || sceneType == NaviSceneId.NAVI_SCENE_3D_CROSS) {
            FloatViewManager.getInstance().notifyCrossImageView(isVisible);
        }
    }

    @Override
    public void onUpdateViaPass() {
        mViewModel.onUpdateViaPass(-1);
    }

    @Override
    public void skipNaviPreferenceScene() {
        mViewModel.showNaviPreferenceScene();
    }

    @Override
    public void skipNaviControlMoreScene() {
        mViewModel.showNaviControlMoreScene();
    }

    @Override
    public void skipNaviControlScene() {
        mViewModel.showNaviControlScene();
    }

    @Override
    public void onChangeNaviPath(long oldPathId, long pathID) {
        mNaviPackage.showSelectPatch(pathID);
    }

    @Override
    public void onSuggestChangePath(long newPathID, long oldPathID,
                                    SuggestChangePathReasonEntity reason) {
        mNaviPackage.showMainAndSuggestPath(newPathID);
    }

    /**
     * @return 返回的是一个包含所有的via点的列表
     */
    public List<NaviViaEntity> getViaList() {
        mViaList.clear();
        if (null == mNaviEtaInfo) {
            return null;
        }
        final List<NaviViaEntity> tmpList = new ArrayList<>();
        //[0]代表起点 [size-1]代表终点
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(
                MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        Logger.i(TAG, "allPoiParamList allPoiParamList:" + allPoiParamList.size());
        for (int i = 1; i < allPoiParamList.size() - 1; i++) {
            final ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = mNaviEtaInfo.viaRemain;
            final RouteParam routeParam = allPoiParamList.get(i);
            Logger.i(TAG, "allPoiParamList viaRemain:" + viaRemain.size());
            if (!ConvertUtils.isEmpty(viaRemain)) {
                final int index = i - 1;
                if (viaRemain.size() > index) {
                    tmpList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, viaRemain.get(index)));
                } else {
                    // 因为有时候是静态的 mNaviEtaInfo.viaRemain数据不会实时刷新，所以就算没有导航数据也要添加
                    tmpList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, null));
                }
            } else {
                tmpList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, null));
            }
        }
        if (mIsShowAutoAdd) {
            tmpList.addAll(mNaviPackage.getAllViaPoints());
            Collections.sort(tmpList, (o1, o2) -> o1.getmArriveTimeStamp() >= o2.getmArriveTimeStamp() ? 1 : -1);
        }
        mViaList.addAll(tmpList);
        // 最后添加终点
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            mViaList.add(NaviDataFormatHelper.getNaviViaEntity(allPoiParamList.get(allPoiParamList.size() - 1), mNaviEtaInfo));
        }
        Logger.i(TAG, "mViaList-Size:" + mViaList.size(), "tmSize:" + tmpList.size());
        return mViaList;
    }

    /**
     * 路由偏好改变回调
     */
    public void onRoutePreferenceChange() {
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        param.setMRouteWay(RouteWayID.ROUTE_WAY_CHANGE_PREFERENCE);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_STRATEGE);
        mRoutePackage.requestRoute(param);
    }

    /**
     * 语音打开/关闭引导中路线全览.
     *
     * @param mapTypeId mapTypeId，对应底图.
     * @param open      true-开启全览  false-关闭全览.
     */
    @Override
    public void onVoiceOverview(final MapType mapTypeId, final boolean open) {
        Logger.i(TAG, "onVoiceOverview open:" + open);
        if (open) {
            mViewModel.naviPreviewSwitch(ENTER_PREVIEW);
        } else {
            mViewModel.naviPreviewSwitch(EXIT_PREVIEW);
        }

    }

    /**
     * 语音切换主辅路、桥上下.
     *
     * @param mapTypeId      MapTypeId，对应底图.
     * @param parallelOption ，切换类型，MAIN-主路 SIDE-辅路  ON-桥上  UNDER-桥下.
     */
    @Override
    public void onVoiceParallelOption(final MapType mapTypeId, final String parallelOption) {
        Logger.i(TAG, "onVoiceParallelOption parallelOption:" + parallelOption);
        if (MAIN_ROAD.equals(parallelOption) || SIDE_ROAD.equals(parallelOption)) {
            mViewModel.naviParallelSwitch(ROAD_SWITCH);
        } else {
            mViewModel.naviParallelSwitch(BRIDGE_SWITCH);
        }
    }

    /**
     * 语音继续导航指令.
     *
     * @param mapTypeId MapTypeId，对应底图.
     */
    @Override
    public void onVoiceContinueNavigation(final MapType mapTypeId) {
        Logger.i(TAG, "onVoiceContinueNavigation");
        mViewModel.naviContinue();
    }

    @Override
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        Logger.i(TAG, "skipNaviSapaDetailScene type:" + type + " sapaInfoEntity:" +
                sapaInfoEntity.toString());
        mViewModel.skipNaviSapaDetailScene(type, sapaInfoEntity);
    }

    /**
     * 将沉浸态状态信息存储到NaviPackage中方便外部接口调用
     *
     * @param imersiveStatus ImmersiveStatus
     */
    private void setImmersiveStatus(final ImersiveStatus imersiveStatus) {
        switch (imersiveStatus) {
            case TOUCH -> {
                NaviPackage.getInstance().setCurrentImmersiveStatus(0);
            }
            case IMERSIVE -> {
                NaviPackage.getInstance().setCurrentImmersiveStatus(1);
            }
            default -> {
                NaviPackage.getInstance().setCurrentImmersiveStatus(-1);
            }
        }
    }

    /**
     * @return true 网络可用，false 网络不可用
     */
    public boolean getNetStatus() {
        // 检查网络状态
        Boolean isNetConnected = mNetWorkUtils.checkNetwork();
        Logger.i(TAG, "getNetStatus isNetConnected:" + isNetConnected);
        return Boolean.TRUE.equals(isNetConnected);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {

    }

    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message,
                                     SearchResultEntity searchResultEntity) {
        Logger.i(TAG, "onSilentSearchResult taskId = " + taskId);
        if (mEndSearchId == taskId) {
            if (null != searchResultEntity &&
                    !ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                drawEndPoint(searchResultEntity.getPoiList().get(0));
            }
        }
    }

    public interface OnNetStatusChangeListener {
        /**
         * @param isConnected true 网络可用，false 网络不可用
         */
        void onNetStatusChange(boolean isConnected);
    }

    /**
     * @param listener OnNetStatusChangeListener
     */
    public void addOnNetStatusChangeListener(final OnNetStatusChangeListener listener) {
        Logger.i(TAG, "addOnNetStatusChangeListener listener = " + listener);
        if (listener != null && !mNetStatusChangeListeners.contains(listener)) {
            mNetStatusChangeListeners.add(listener);
        }
    }

    /**
     * @param listener OnNetStatusChangeListener
     */
    public void removeOnNetStatusChangeListener(final OnNetStatusChangeListener listener) {
        Logger.i(TAG, "removeOnNetStatusChangeListener listener = " + listener);
        mNetStatusChangeListeners.remove(listener);
    }

    /**
     * 网络状态改变
     */
    private void onNetStatusChange() {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                Boolean isNetConnected = mNetWorkUtils.checkNetwork();
                Logger.i(TAG, "onNetStatusChange isNetConnected:" + isNetConnected);
                if (!ConvertUtils.isEmpty(mNetStatusChangeListeners)) {
                    if (isNetConnected != mCurrentNetStatus) {
                        mCurrentNetStatus = isNetConnected;
                        for (OnNetStatusChangeListener listener : mNetStatusChangeListeners) {
                            listener.onNetStatusChange(Boolean.TRUE.equals(isNetConnected));
                        }
                    }
                }
            }
        }, ONE_SECOND);
    }

    /**
     * 当前网络状态
     *
     * @return
     */
    public boolean isNetConnected() {
        return mCurrentNetStatus;
    }

    @Override
    public void onNetConnectSuccess() {
        Logger.i(TAG, "onNetConnectSuccess");
        onNetStatusChange();
    }

    @Override
    public void onNetUnavailable() {
        Logger.i(TAG, "onNetUnavailable");
        onNetStatusChange();
    }

    @Override
    public void onNetBlockedStatusChanged() {
        Logger.i(TAG, "onNetBlockedStatusChanged");
        onNetStatusChange();
    }

    @Override
    public void onNetLosing() {
        Logger.i(TAG, "onNetLosing");
        onNetStatusChange();
    }

    @Override
    public void onNetLinkPropertiesChanged() {
        Logger.i(TAG, "onNetLinkPropertiesChanged");
        onNetStatusChange();
    }

    @Override
    public void onNetDisConnect() {
        Logger.i(TAG, "onNetDisConnect");
        onNetStatusChange();
    }

    /***
     * 这里主要是 透出预计到达时间
     * @param infos
     */
    @Override
    public void onUpdateElectVehicleETAInfo(List<FyElecVehicleETAInfo> infos) {
        IGuidanceObserver.super.onUpdateElectVehicleETAInfo(infos);
        Logger.i(TAG, "onUpdateElectVehicleETAInfo");
        if (mTipManager != null) {
            mTipManager.onUpdateElectVehicleETAInfo(infos);
        }
        if (null != mViewModel) {
            mViewModel.onUpdateElectVehicleETAInfo(infos);
        }
    }

    @Override
    public void onUpdateChargeStationPass(long viaIndex) {
        IGuidanceObserver.super.onUpdateChargeStationPass(viaIndex);
        // 将要通过的充电桩回调
        if (mTipManager != null) {
            mTipManager.onUpdateChargeStationPass();
        }
    }

    /**
     * 显示控制详情
     */
    @Override
    public void showControlDetails() {
        mViewModel.showControlDetails();
    }

    @Override
    public void onShowNaviWeather(RouteWeatherInfo info) {
        IGuidanceObserver.super.onShowNaviWeather(info);
        Logger.i(TAG, "onShowNaviWeather");
        final MessageCenterInfo centerInfo = new MessageCenterInfo();
        centerInfo.setMsgTitle(info.getMWeatherName());
        centerInfo.setMsgContent(info.getMText());
        centerInfo.setSrcImg(R.drawable.img_message_center_weather);
        centerInfo.setMsgType(MessageCenterType.WEATHER);
        messageCenterManager.pushMessage(centerInfo);
    }

    private void showDeleteAllTip(final NaviViaEntity entity) {
        Logger.i(TAG, "showDeleteAllTip");
        mViewModel.showDeleteAllTip();
    }

    public void deleteAutoAddChargeStation() {
        mIsShowAutoAdd = false;
        mViewModel.onUpdateViaList(mIsShowAutoAdd);
    }

    /**
     * @param keyWord    搜索关键字
     * @param searchType 搜索类型
     */
    @Override
    public void goSearchView(String keyWord, int searchType) {
        ISceneCallback.super.goSearchView(keyWord, searchType);
        mViewModel.goSearchView(keyWord, searchType);
    }

    /**
     * 跳转到沿途搜页面
     */
    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_POI_ROUTE)
    public void goAlongWayList() {
        ISceneCallback.super.goAlongWayList();
        mViewModel.goAlongWayList();
    }

    @Override
    public void closeSearchView() {
        ISceneCallback.super.closeSearchView();
        mViewModel.closeSearchView();
    }

    @Override
    public void onRouteSuccess(String successMsg) {
        Logger.i(TAG, "onRouteSuccess");
        mViewModel.updateViaList();
        // 如果是预览状态，还是进入预览
        if (mNaviPackage.getPreviewStatus()) {
            OpenApiHelper.enterPreview(MapType.MAIN_SCREEN_MAIN_MAP);
        }
    }

    @Override
    public void onRouteItemClick(MapType mapTypeId, LayerPointItemType type, LayerItemRoutePointClickResult result) {
        Logger.i(TAG, "onRouteItemClick result = " + result.toString() + " type = " + type);
        if (type == LayerPointItemType.ROUTE_PATH) {
            int currentNaviType = mNaviPackage.getCurrentNaviType();
            if (currentNaviType != 0) {
                Logger.i(TAG, "非GPS 导航，不支持手动切换路线");
                return;
            }
            if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                Logger.i(TAG, "离线状态，不支持手动切换路线");
                return;
            }
            long pathId = result.getIndex();
            int pathIndex = OpenApiHelper.getPathIndex(pathId);
            mNaviPackage.selectPath(MapType.MAIN_SCREEN_MAIN_MAP, pathId, pathIndex);
        }
    }

    @Override
    public boolean getCurrentFragmentIsNavi() {
        Fragment fragment = StackManager.getInstance().
                getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name());
        return fragment instanceof NaviGuidanceFragment;
    }

    @Override
    public void hideNaviContent() {
        if (null != mViewModel) {
            mViewModel.hideNaviContent();
        }
    }

    @Override
    public boolean isNeedPreViewShowList() {
        if (mViewModel != null) {
            return mViewModel.isNeedPreViewShowList();
        }
        return false;
    }

    public void backToNaviFragment() {
        mViewModel.backToNaviFragment();
    }

    public String getPlateNumber() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
    }

    public String getAvoidLimit() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
    }

    public String getPreferences() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE);
    }

    public String getEnergy() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN);
    }

    @Override
    public void stopSpeech() {
        if (null != mNaviPackage) {
            mNaviPackage.stopSpeech();
        }
    }

    @Override
    public void updateNextIcon(int resource, BitmapDrawable drawable) {
        if (null != mNextManeuverEntity) {
            mNextManeuverEntity.setNextIconResource(resource);
            mNextManeuverEntity.setNextIconDrawable(drawable);
        }
    }

    @Override
    public void updateNextStatus(boolean isVisible, boolean isOffLine) {
        if (null != mNextManeuverEntity) {
            mNextManeuverEntity.setNextManeuverVisible(isVisible);
            mNextManeuverEntity.setNextManeuverOffLine(isOffLine);
        }
    }

    @Override
    public NextManeuverEntity getNextManeuverEntity() {
        return mNextManeuverEntity;
    }

    @Override
    public void updateNextText(String text) {
        if (null != mNextManeuverEntity) {
            mNextManeuverEntity.setNextText(text);
        }
    }
}
