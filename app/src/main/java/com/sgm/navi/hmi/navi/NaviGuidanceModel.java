package com.sgm.navi.hmi.navi;


import android.app.Activity;
import android.graphics.drawable.BitmapDrawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MotionEvent;

import androidx.fragment.app.Fragment;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.launcher.LauncherWindowService;
import com.sgm.navi.hmi.poi.PoiDetailsFragment;
import com.sgm.navi.hmi.search.alongway.MainAlongWaySearchFragment;
import com.sgm.navi.hmi.search.searchresult.SearchResultFragment;
import com.sgm.navi.hmi.setting.SettingFragment;
import com.sgm.navi.hmi.splitscreen.SRFloatWindowService;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneManager;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapStateStyle;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.MapTypeManager;
import com.sgm.navi.service.define.message.MessageCenterInfo;
import com.sgm.navi.service.define.message.MessageCenterType;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.FyElecVehicleETAInfo;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.navi.NextManeuverEntity;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navi.SuggestChangePathReasonEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.FyRouteOption;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteAlterChargeStationParam;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePreferenceID;
import com.sgm.navi.service.define.route.RoutePriorityType;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RouteWayID;
import com.sgm.navi.service.define.route.RouteWeatherInfo;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.message.MessageCenterManager;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.base.BaseModel;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.utils.ClusterMapOpenCloseListener;
import com.sgm.navi.utils.ClusterMapOpenCloseManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledFuture;
public class NaviGuidanceModel extends BaseModel<NaviGuidanceViewModel> implements
        IGuidanceObserver, ImmersiveStatusScene.IImmersiveStatusCallBack, ISceneCallback,
        IRouteResultObserver, NetWorkUtils.NetworkObserver, ILayerPackageCallBack,
        SearchResultCallback, ClusterMapOpenCloseListener, SettingPackage.SettingChangeCallback,
        IMapPackageCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_MODEL;
    private final NaviPackage mNaviPackage;
    private final RoutePackage mRoutePackage;
    private final LayerPackage mLayerPackage;
    private final MapPackage mMapPackage;
    private final SettingPackage mSettingPackage;
    private final MessageCenterManager messageCenterManager;
    private final ClusterMapOpenCloseManager mClusterMapOpenCloseManager;
    private final NaviStatusPackage mNaviSatusPackage;
    private ImersiveStatus mCurrentStatus = ImersiveStatus.IMERSIVE;
    private List<NaviViaEntity> mViaList = new ArrayList<>();
    private NaviEtaInfo mNaviEtaInfo;
    private Runnable mEndPoiSearchRunnable;
    private Runnable mCloseClusterOverView;
    private Runnable mOnClusterMapOpenOrClose;
    private Runnable mFirstDrawEndPoint;
    private Runnable mInitLazyView;
    private Runnable mUpdateViaList;
    private Runnable mShowPreView;
    private volatile boolean mIsClusterOpen;
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

    private boolean mIsViaEmpty = true;
    private List<OnNetStatusChangeListener> mNetStatusChangeListeners =
            new CopyOnWriteArrayList<>();

    private final SearchPackage mSearchPackage;
    private ChargeTipManager mTipManager;
    private ViaListManager mViaListManager;
    // 路口大图是否显示
    private boolean mIsShowCrossImage;
    // 记录路口大图出现时的起始距离
    private int mMoveStartDistance;
    private NextManeuverEntity mNextManeuverEntity;
    private boolean mIsNeedUpdateViaList;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;
    private float mDownX, mDownY;
    private long mDownTime;
    private static final int CLICK_THRESHOLD = 200; // ms
    private static final int MOVE_THRESHOLD = 20; // px
    private long mCurrentViaIndex;
    // 记录手动提前到达的途经点名称
    private String mArrivedViaPoiId;
    private String mSuccessMsg = "";
    private boolean mIsAutoReRoute = true;
    private static final int ONE_ROUTE_LINE_DISTANCE = 150 * 1000; //超过150公里不显示备选路线

    public NaviGuidanceModel() {
        mMapPackage = MapPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mClusterMapOpenCloseManager = ClusterMapOpenCloseManager.getInstance();
        mNaviSatusPackage = NaviStatusPackage.getInstance();
        mNetWorkUtils = NetWorkUtils.Companion.getInstance();
        messageCenterManager = MessageCenterManager.getInstance();
        mModelHelp = new NaviGuidanceHelp();
    }

    /**
     * 终点搜，为了引导的终点扎标，显示营业时间/充电站信息
     */
    private void endPoiSearch() {
        Logger.i(TAG, "endPoiSearch");
        RouteParam endRouteParam = mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
        if (endRouteParam == null) {
            Logger.i(TAG, "endRouteParam is null");
            return;
        }
        String poiId = endRouteParam.getPoiID();
        mEndSearchId = mSearchPackage.poiIdSearch(poiId, true);
        Logger.i(TAG, "mEndSearchId = ", mEndSearchId, " poiId = ", poiId);
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
        mSettingPackage.setSettingChangeCallback(TAG, this);
        mMapPackage.registerCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mClusterMapOpenCloseManager.addClusterMapOpenCloseListener(this);
        initRunnable();
        // 因为生命周期是和HMI绑定的，如果页面重启并且是在导航台进入三分钟一次的终点POI查询
        if (NaviStatus.NaviStatusType.NAVING.equals(
                mNaviSatusPackage.getCurrentNaviStatus())) {
            ThreadManager.getInstance().removeHandleTask(mEndPoiSearchRunnable);
            ThreadManager.getInstance().postUi(mEndPoiSearchRunnable);
        }
        mNextManeuverEntity = new NextManeuverEntity();
    }

    private void initRunnable() {
        mEndPoiSearchRunnable = new Runnable() {
            @Override
            public void run() {
                try {
                    endPoiSearch();
                    ThreadManager.getInstance().removeHandleTask(mEndPoiSearchRunnable);
                    ThreadManager.getInstance().
                            postDelay(this,
                                    NumberUtils.NUM_3 * NumberUtils.NUM_60 * NumberUtils.NUM_1000);
                } catch (Exception e) {
                    Logger.e(TAG, "mEndPoiSearchRunnable ", e.getMessage());
                }
            }
        };
        mCloseClusterOverView = new Runnable() {
            @Override
            public void run() {
                try {
                    boolean overViewFix = mNaviPackage.getFixedOverViewStatus();
                    Logger.i(TAG, " overViewFix = ", overViewFix);
                    if (!overViewFix) {
                        if (mViewModel != null) {
                            mViewModel.naviPreviewSwitch(NumberUtils.NUM_0);
                        }
                    }
                } catch (Exception e) {
                    Logger.e(TAG, "mCloseClusterOverView ", e.getMessage());
                }
            }
        };
        mOnClusterMapOpenOrClose = new Runnable() {
            @Override
            public void run() {
                try {
                    if (mIsClusterOpen) {
                        if (NaviStatus.NaviStatusType.NAVING.equals(mNaviSatusPackage.getCurrentNaviStatus())) {
                            openClusterOverView();
                        }
                    } else {
                        if (NaviStatus.NaviStatusType.NAVING.equals(mNaviSatusPackage.getCurrentNaviStatus()) &&
                                mNaviPackage.getClusterFixOverViewStatus()) {
                            closeClusterOverViewInEightSec();
                        }
                    }
                } catch (Exception e) {
                    Logger.e(TAG, "onClusterMapOpenOrClose Exception:", e.getMessage());
                }
            }
        };
        mInitLazyView = new Runnable() {
            @Override
            public void run() {
                if (mViewModel != null) {
                    mViewModel.initLazyView();
                }
            }
        };
        mFirstDrawEndPoint = new Runnable() {
            @Override
            public void run() {
                endPoiSearch();
            }
        };
        mUpdateViaList = new Runnable() {
            @Override
            public void run() {
                try {
                    if (mViewModel != null) {
                        mViewModel.updateViaList();
                    }
                } catch (Exception e) {
                    Logger.e(TAG, "mUpdateViaList Exception:", e.getMessage());
                }
            }
        };
        mShowPreView = new Runnable() {
            @Override
            public void run() {
                openClusterOverView();
            }
        };
    }

    private void releaseRunnable() {
        ThreadManager.getInstance().removeHandleTask(mEndPoiSearchRunnable);
        ThreadManager.getInstance().removeHandleTask(mCloseClusterOverView);
        ThreadManager.getInstance().removeHandleTask(mOnClusterMapOpenOrClose);
        ThreadManager.getInstance().removeHandleTask(mInitLazyView);
        ThreadManager.getInstance().removeHandleTask(mFirstDrawEndPoint);
        ThreadManager.getInstance().removeHandleTask(mUpdateViaList);
        ThreadManager.getInstance().removeHandleTask(mShowPreView);
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
            mCurrentViaIndex = 0;
            mArrivedViaPoiId = "";
            final boolean isAutoScale = SettingPackage.getInstance().getAutoScale();
            if (isAutoScale) {
                mLayerPackage.openDynamicLevel(MapType.MAIN_SCREEN_MAIN_MAP,
                        DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
            }
            // 延时绘制终点扎标，减少峰值消耗
            ThreadManager.getInstance().postDelay(mFirstDrawEndPoint, NumberUtils.NUM_1000);
            // 开始三分钟查询一次终点POI信息
            ThreadManager.getInstance().removeHandleTask(mEndPoiSearchRunnable);
            ThreadManager.getInstance().postUi(mEndPoiSearchRunnable);
            mIsShowAutoAdd = true;
            final MapType mapTypeId = MapTypeManager.getInstance().
                    getMapTypeIdByName(mViewModel.mScreenId);
            mNaviPackage.addNaviRecord(false);
            mLayerPackage.setStartPointVisible(mapTypeId, false);
            mMapPackage.setMapStateStyle(MapType.MAIN_SCREEN_MAIN_MAP,
                    MapStateStyle.MAP_NAVI);
            if (!mClusterMapOpenCloseManager.isClusterOpen()) {
                //全览之后开启导航，需要取消全览
                mMapPackage.exitPreview(MapType.MAIN_SCREEN_MAIN_MAP,
                        DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, true);
                mNaviPackage.setClusterFixOverViewStatus(false);
                mMapPackage.goToCarPosition(mapTypeId);
                mLayerPackage.setFollowMode(mapTypeId, true);
                mNaviPackage.setRouteEnergyEmptyPointVisible(MapType.MAIN_SCREEN_MAIN_MAP,
                        false);
            } else {
                mNaviPackage.setClusterFixOverViewStatus(true);
                mNaviPackage.setPreviewStatus(true);
                ThreadManager.getInstance().postDelay(mShowPreView, NumberUtils.NUM_100);
            }
        }
    }

    /**
     * 开启由于仪表打开地图视图的全览
     */
    private void openClusterOverView() {
        boolean isClusterMapOpen = mClusterMapOpenCloseManager.isClusterOpen();
        Logger.i(TAG, "openClusterOverView isClusterMapOpen = ", isClusterMapOpen);
        if (isClusterMapOpen) {
            cancelClusterOverViewTimer();
            if (mViewModel != null) {
                mViewModel.naviPreviewSwitch(NumberUtils.NUM_1);
            }
            mNaviPackage.setClusterFixOverViewStatus(true);
        }
    }

    /**
     * 延时八秒关闭仪表地图触发的全览
     */
    private void closeClusterOverViewInEightSec() {
        mNaviPackage.setClusterFixOverViewStatus(false);
        initTimer();
    }

    /**
     * 开始倒计时
     */
    public void initTimer() {
        Logger.i(TAG, "initTimer");
        cancelClusterOverViewTimer();
        mTimes = NumberUtils.NUM_8;
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(mCloseClusterOverView);
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    /**
     * 取消仪表退出八秒退出全览的倒计时
     */
    @Override
    public void cancelClusterOverViewTimer() {
        Logger.i(TAG, "cancelClusterOverViewTimer");
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    /**
     * 引导终点扎标绘制
     */
    private void drawEndPoint(PoiInfoEntity poiInfoEntity) {
        if (null != poiInfoEntity) {
            LayerItemRouteEndPoint endPoint = new LayerItemRouteEndPoint();
            endPoint.setEndPointType(LayerPointItemType.ROUTE_POINT_END_BUSINESS_HOURS);
            List<ChargeInfo> chargeInfoList = poiInfoEntity.getChargeInfoList();
            ChargeInfo chargeInfo = chargeInfoList == null ? null : chargeInfoList.get(0);
            String businessTime = poiInfoEntity.getBusinessTime();
            Logger.i(TAG, "businessTime = ", businessTime);
            int carType = OpenApiHelper.powerType();
            if (!ConvertUtils.isEmpty(chargeInfo) && carType == 1) {
                int slowTotal = chargeInfo.getMSlowTotal();
                int slowFree = chargeInfo.getMSlowFree();
                int fastTotal = chargeInfo.getMFastTotal();
                int fastFree = chargeInfo.getMFastFree();
                Logger.i(TAG, "slowTotal = ", slowTotal, " slowFree = ",
                        slowFree, " fastTotal = ", fastTotal, " fastFree = ", fastFree);
                String chargeInfoStr = getChargeInfo(fastFree, fastTotal, slowFree, slowTotal);
                if (TextUtils.isEmpty(chargeInfoStr) && !TextUtils.isEmpty(businessTime)) {
                    endPoint.setBusinessHours(businessTime);
                    mNaviPackage.setEndPoint(endPoint);
                    return;
                }
                endPoint.setBusinessHours(chargeInfoStr);
                mNaviPackage.setEndPoint(endPoint);
            } else if (!TextUtils.isEmpty(businessTime)) {
                endPoint.setBusinessHours(businessTime);
                mNaviPackage.setEndPoint(endPoint);
            }
        }
    }

    private String getChargeInfo(int fastFree, int fastTotal, int slowFree, int slowTotal) {
        StringBuilder sb = new StringBuilder();

        boolean hasFast = fastTotal > 0;
        boolean hasSlow = slowTotal > 0;

        if (hasFast) {
            if (fastFree > 0) {
                sb.append(ResourceUtils.Companion.getInstance().getString(R.string.navi_quick)).
                        append(fastFree).append(ResourceUtils.Companion.getInstance().
                                getString(com.sgm.navi.scene.R.string.slash)).append(fastTotal);
            } else {
                sb.append(ResourceUtils.Companion.getInstance().getString(R.string.navi_quick)).
                        append(fastTotal);
            }
        }

        if (hasSlow) {
            if (sb.length() > 0) sb.append(" ");
            if (slowFree > 0) {
                sb.append(ResourceUtils.Companion.getInstance().getString(R.string.navi_slow)).
                        append(slowFree).append(ResourceUtils.Companion.getInstance().
                        getString(com.sgm.navi.scene.R.string.slash)).append(slowTotal);
            } else {
                sb.append(ResourceUtils.Companion.getInstance().getString(R.string.navi_slow)).
                        append(slowTotal);
            }
        }

        return sb.length() == 0 ? "" : sb.toString();
    }

    @Override
    public void onAttachViewModel(final NaviGuidanceViewModel baseViewModel) {
        super.onAttachViewModel(baseViewModel);
        mViewModel.addSceneCallback(this);
        mTipManager = new ChargeTipManager(mViewModel);
        mViaListManager = new ViaListManager(this);
    }

    @Override
    public void onNaviInfo(final NaviEtaInfo naviInfoBean) {
        if (ConvertUtils.isEmpty(naviInfoBean)) return;
        checkShowViaDetail(naviInfoBean);
        mNaviEtaInfo = naviInfoBean;
        if (mIsNeedUpdateViaList && !ConvertUtils.isNull(mViewModel)) {
            mViewModel.updateViaListImm();
        }
        if (!ConvertUtils.isNull(mViewModel)) {
            mViewModel.onNaviInfo(naviInfoBean);
        }
        if (!ConvertUtils.isNull(mTipManager)) {
            mTipManager.setNextViaChargeStation(naviInfoBean);
        }
        if (!ConvertUtils.isNull(mViaListManager)) {
            mViaListManager.onNaviInfo(naviInfoBean);
        }
        if (mIsShowCrossImage) {
            // 获得行驶过的距离
            int moveDistance = mMoveStartDistance - naviInfoBean.getRemainDist();
            mViewModel.onCrossProgress(moveDistance);
        } else {
            mMoveStartDistance = naviInfoBean.getRemainDist();
        }
    }

    private void checkShowViaDetail(final NaviEtaInfo naviInfoBean) {
        ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = naviInfoBean.getViaRemain();
        if (!ConvertUtils.isEmpty(viaRemain)) {
            mIsViaEmpty = false;
            NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = viaRemain.get(0);
            if (null != naviTimeAndDist) {
                int distance = naviTimeAndDist.dist;
                if (distance <= NumberUtils.NUM_5000) {
                    if (null != mViewModel) {
                        mViewModel.showViaDetail(true);
                    }
                } else {
                    if (null != mViewModel) {
                        mViewModel.showViaDetail(false);
                    }
                }
            }
        } else {
            if (mIsViaEmpty) {
                return;
            }
            mIsViaEmpty = true;
            mViewModel.showViaDetail(false);
        }
    }

    @Override
    public void onNaviStop() {
        mNaviPackage.setEndPoint(null);
        closeNavi();
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
        if (ConvertUtils.isEmpty(naviTmcInfo)) {
            Logger.i(TAG, "onUpdateTMCLightBar naviTmcInfo is null");
            return;
        }
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
        Logger.i(TAG, "onUpdate viaIndex = " + viaIndex);
        mCurrentViaIndex = viaIndex + 1;
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
            if (!ConvertUtils.isNull(mViewModel)) {
                mViewModel.updateViaListImm();
            }
            Logger.i(TAG, "onUpdateViaPass isDeleteSuccess = ", isDeleteSuccess);
        }
        if (!ConvertUtils.isNull(mViewModel)) {
            mViewModel.onUpdateViaPass(viaIndex);
        }
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_NEWROUTE)
    public void onSelectMainPathStatus(final long pathID, final int result) {
        Logger.i(TAG, "onSelectMainPathStatus pathID = ", pathID, " result = ", result);
        if (result == NaviConstant.ChangeNaviPathResult.CHANGE_NAVI_PATH_RESULT_SUCCESS) {
            mNaviPackage.onlyShowCurrentPath();
        }
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        if (ConvertUtils.isEmpty(laneInfo)) {
            Logger.i(TAG, "onLaneInfo laneInfo is null");
            return;
        }
        mViewModel.onLaneInfo(isShowLane, laneInfo);
    }

    @Override
    public void onRouteResult(final RequestRouteResult requestRouteResult) {
        RouteWayID routeWayID = requestRouteResult.getMRouteWay();
        mIsAutoReRoute = requestRouteResult.isMAutoRouting();
        Logger.i(TAG, "onRouteResult routeWayID = ", routeWayID);
        if (routeWayID.equals(RouteWayID.ROUTE_WAY_ADD_VIA) ||
                routeWayID.equals(RouteWayID.ROUTE_WAY_ADD_ALL_VIA) ||
                routeWayID.equals(RouteWayID.ROUTE_WAY_DELETE_VIA) ||
                routeWayID.equals(RouteWayID.ROUTE_WAY_SORT_VIA)) {
            ThreadManager.getInstance().postDelay(mUpdateViaList, NumberUtils.NUM_500);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        cancelClusterOverViewTimer();
        NaviSceneManager.getInstance().destroySceneView();
        mNetWorkUtils.unRegisterNetworkObserver(this);
        ImmersiveStatusScene.getInstance().unRegisterCallback("NaviGuidanceModel");
        mSearchPackage.unRegisterCallBack(NaviConstant.KEY_NAVI_MODEL);
        mLayerPackage.unRegisterCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mRoutePackage.unRegisterRouteObserver(NaviConstant.KEY_NAVI_MODEL);
        mSettingPackage.unRegisterSettingChangeCallback(TAG);
        mMapPackage.unRegisterCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mClusterMapOpenCloseManager.removeListener(this);
        if (mNaviPackage != null) {
            mNaviPackage.unregisterObserver(NaviConstant.KEY_NAVI_MODEL);
        }
        if (mTipManager != null) {
            mTipManager.unInit();
        }
        releaseRunnable();
        mEndPoiSearchRunnable = null;
        mCloseClusterOverView = null;
        mOnClusterMapOpenOrClose = null;
    }

    @Override
    public void onImmersiveStatusChange(final MapType mapTypeId,
                                        final ImersiveStatus currentImersiveStatus) {
        if (!NaviStatus.NaviStatusType.NAVING.equals(mNaviSatusPackage.
                getCurrentNaviStatus())) {
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
        if (!mSettingPackage.getPrivacyStatus()) {
            Logger.i(TAG, " PrivacyStatus is false");
            ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getText(com.sgm.navi.scene.R.string.open_privacy_permission));
            return;
        }
        Logger.i(TAG, "skipAlongWayFragment");
        addFragment(new MainAlongWaySearchFragment(), new Bundle());
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
        // 开启补能规划，重新算路
        try {
            ThreadManager.getInstance().execute(() -> {
                SettingPackage.getInstance().setChargingPlan(true);
                final RouteRequestParam param = new RouteRequestParam();
                param.setMRouteWay(RouteWayID.ROUTE_WAY_REFRESH);
                param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH);
                mRoutePackage.requestRoute(param);
            });
        } catch (Exception e) {
            Logger.e(TAG, "openSupplyPlan error: ", e.getMessage());
        }
    }

    /**
     * 添加途径点
     *
     * @param info 替换充电站信息
     */
    @Override
    public void addViaList(final RouteAlterChargeStationInfo info) {
        if (ConvertUtils.isEmpty(info)) {
            Logger.i(TAG, "info == null");
            return;
        }
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        final GeoPoint geoPoint = new GeoPoint(info.getMPos().getLon(), info.getMPos().getLat(), info.getMPos().getZ());
        poiInfoEntity.setPid(info.getMPoiId());
        poiInfoEntity.setName(info.getMName());
        poiInfoEntity.setTypeCode("011100");
        poiInfoEntity.setPoint(geoPoint);
        //TODO 回调
        mRoutePackage.addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
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
        addFragment(new SettingFragment(), new Bundle());
    }

    @Override
    public void deleteViaPoint(final NaviViaEntity entity) {
        if (!mSettingPackage.getPrivacyStatus()){
            Logger.i(TAG, " PrivacyStatus is false");
            ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getText(com.sgm.navi.scene.R.string.open_privacy_permission));
            return;
        }
        ISceneCallback.super.deleteViaPoint(entity);
        if (entity.getChargeInfo() != null && entity.getChargeInfo().isAutoAdd()) {
            showDeleteAllTip(entity);
        } else {
            final PoiInfoEntity poiInfo = new PoiInfoEntity();
            poiInfo.setPoint(entity.getRealPos());
            poiInfo.setPid(entity.getPid());
            poiInfo.setName(entity.getName());
            poiInfo.setAddress(entity.getAddress());
            final boolean result = mRoutePackage.removeVia(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), poiInfo, true);
            mViewModel.notifyDeleteViaPointResult(result, entity);
        }
    }

    @Override
    public void updateSceneVisible(final NaviSceneId sceneType, final boolean isVisible) {
        if (mViewModel != null) {
            mViewModel.updateSceneVisible(sceneType, isVisible);
        }
        if (sceneType == NaviSceneId.NAVI_SCENE_2D_CROSS || sceneType == NaviSceneId.NAVI_SCENE_3D_CROSS) {
            LauncherWindowService.getInstance().changeCrossVisible(isVisible);
            SRFloatWindowService.getInstance().changeCrossVisible(isVisible);
        }
    }

    @Override
    public void onUpdateViaPass() {
        mArrivedViaPoiId = getNextViaPoiId();
        // 清楚图层的途经点扎标
        mNaviPackage.removeViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, mCurrentViaIndex + "");
        // 清楚tbt面板的途经点信息
        if (!ConvertUtils.isNull(mViewModel)) {
            mViewModel.onNaviInfoByViaArrived(mNaviEtaInfo);
            mViewModel.onUpdateViaPass(-1);
        }
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

    @Override
    public Activity getActivity() {
        return mViewModel.getActivity();
    }

    /**
     * @return 返回的是一个包含所有的via点的列表
     */
    public List<NaviViaEntity> getViaList() {
        mViaList.clear();
        mIsNeedUpdateViaList = false;
        if (null == mNaviEtaInfo) {
            Logger.e(TAG, "getViaList mNaviEtaInfo is null");
            return null;
        }
        final List<NaviViaEntity> tmpList = new ArrayList<>();
        //[0]代表起点 [size-1]代表终点
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(
                MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        Logger.i(TAG, "allPoiParamList allPoiParamList:", allPoiParamList.size());
        for (int i = 1; i < allPoiParamList.size() - 1; i++) {
            final ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = mNaviEtaInfo.viaRemain;
            final RouteParam routeParam = allPoiParamList.get(i);
            if (!ConvertUtils.isEmpty(viaRemain)) {
                final int index = i - 1;
                Logger.i(TAG, "allPoiParamList viaRemain:", viaRemain.size());
                Logger.i(TAG, "allPoiParamList index:", index);
                if (viaRemain.size() > index) {
                    tmpList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, viaRemain.get(index), true, false));
                } else {
                    // 因为有时候是静态的 mNaviEtaInfo.viaRemain数据不会实时刷新，所以就算没有导航数据也要添加
                    tmpList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, null, true, false));
                    mIsNeedUpdateViaList = true;
                }
            } else {
                tmpList.add(NaviDataFormatHelper.getNaviViaEntity(routeParam, null, true, false));
                mIsNeedUpdateViaList = true;
            }
        }
        if (mIsShowAutoAdd) {
            tmpList.addAll(mNaviPackage.getAllViaPoints());
            Collections.sort(tmpList, (o1, o2) -> o1.getmArriveTimeStamp() >= o2.getmArriveTimeStamp() ? 1 : -1);
        }
        mViaList.addAll(tmpList);
        // 最后添加终点
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            mViaList.add(NaviDataFormatHelper.getNaviViaEntity(
                    allPoiParamList.get(allPoiParamList.size() - 1), mNaviEtaInfo, true, true));
        }
        Logger.i(TAG, "mViaList-Size:", mViaList.size(), "tmSize:", tmpList.size());
        if (!ConvertUtils.isEmpty(mViaList)) {
            NaviViaEntity naviViaEntity = mViaList.get(0);
            updateNewestViaPoint(naviViaEntity);
        }
        return mViaList;
    }

    private void updateNewestViaPoint(NaviViaEntity naviViaEntity) {
        if (null != mViewModel) {
            mViewModel.updateNewestViaPoint(naviViaEntity);
        }
    }

    /**
     * 路由偏好改变回调
     */
    public void onRoutePreferenceChange() {
        ThreadManager.getInstance().execute(() -> {
            final RouteRequestParam param = new RouteRequestParam();
            param.setMMapTypeId(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
            param.setMRouteWay(RouteWayID.ROUTE_WAY_CHANGE_PREFERENCE);
            param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_STRATEGE);
            mRoutePackage.requestRoute(param);
        });
    }

    /**
     * 语音打开/关闭引导中路线全览.
     *
     * @param mapTypeId mapTypeId，对应底图.
     * @param open      true-开启全览  false-关闭全览.
     */
    @Override
    public void onVoiceOverview(final MapType mapTypeId, final boolean open) {
        Logger.i(TAG, "onVoiceOverview open:", open);
        if (open) {
            if (mViewModel != null) {
                mViewModel.naviPreviewSwitch(ENTER_PREVIEW);
            }
        } else {
            if (mViewModel != null) {
                mViewModel.naviPreviewSwitch(EXIT_PREVIEW);
            }
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
        Logger.i(TAG, "onVoiceParallelOption parallelOption:", parallelOption);
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
        //Logger.i(TAG, "skipNaviSapaDetailScene type:", type, " sapaInfoEntity:", sapaInfoEntity.toString());
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
        Logger.i(TAG, "getNetStatus isNetConnected:", isNetConnected);
        return Boolean.TRUE.equals(isNetConnected);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {

    }

    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        Logger.i(TAG, "onSilentSearchResult taskId = ", taskId);
        if (mEndSearchId == taskId) {
            if (null != searchResultEntity &&
                    !ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                drawEndPoint(searchResultEntity.getPoiList().get(0));
            }
        }
        if (mTipManager != null) {
            mTipManager.onSilentSearchResult(taskId, errorCode, message, searchResultEntity);
        }
        if (mViaListManager != null) {
            mViaListManager.onSilentSearchResult(taskId, errorCode, message, searchResultEntity);
        }
    }

    @Override
    public void onNetSearchResult(int taskId, String searchKey, BaseRep result) {
        if (mTipManager != null) {
            mTipManager.onNetSearchResult(taskId, searchKey, result);
        }
    }

    @Override
    public void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {
        if (mTipManager != null) {
            mTipManager.onRouteAlterChargeStationInfo(routeAlterChargeStationParam);
        }
    }

    @Override
    public void onClusterMapOpenOrClose(boolean isOpen) {
        Logger.i(TAG, "onClusterMapOpenOrClose isOpen:", isOpen);
        mIsClusterOpen = isOpen;
        ThreadManager.getInstance().postUi(mOnClusterMapOpenOrClose);
    }

    public void restoreNavigationByRebuild() {
        mNaviPackage.restoreNavigationByRebuild();
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
        if (listener != null && !mNetStatusChangeListeners.contains(listener)) {
            mNetStatusChangeListeners.add(listener);
        }
    }

    /**
     * @param listener OnNetStatusChangeListener
     */
    public void removeOnNetStatusChangeListener(final OnNetStatusChangeListener listener) {
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
                Logger.i(TAG, "onNetStatusChange isNetConnected:", isNetConnected);
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
        Logger.i(TAG, "isNetConnected mCurrentNetStatus = ", mCurrentNetStatus);
        return mCurrentNetStatus;
    }

    @Override
    public void onNetConnectSuccess() {
        Logger.i(TAG, "onNetConnectSuccess");
        onNetStatusChange();
    }

    @Override
    public void onNetUnavailable() {
        Logger.d(TAG, "onNetUnavailable");
        onNetStatusChange();
    }

    @Override
    public void onNetBlockedStatusChanged() {
        Logger.d(TAG, "onNetBlockedStatusChanged");
        onNetStatusChange();
    }

    @Override
    public void onNetLosing() {
        Logger.d(TAG, "onNetLosing");
        onNetStatusChange();
    }

    @Override
    public void onNetLinkPropertiesChanged() {
        Logger.d(TAG, "onNetLinkPropertiesChanged");
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
        if (mTipManager != null) {
            mTipManager.onUpdateChargeStationPass(viaIndex);
        }
    }

    public void updateViaListState(List<NaviViaEntity> list) {
        if (mTipManager != null) {
            mTipManager.updateViaList(list);
        }
        if (mViaListManager != null) {
            mViaListManager.updateViaList(list);
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

    @Override
    public void onMeterAction() {
        mViewModel.onMeterAction();
    }

    private void showDeleteAllTip(final NaviViaEntity entity) {
        Logger.i(TAG, "showDeleteAllTip");
        mViewModel.showDeleteAllTip();
    }

    public void deleteAutoAddChargeStation() {
        mIsShowAutoAdd = false;
        mViewModel.onUpdateViaList(mIsShowAutoAdd);
        MapPackage.getInstance().resetTickCount(MapType.MAIN_SCREEN_MAIN_MAP,2);
    }

    /**
     * 删除所有补能规划扎标
     */
    public void clearAllViaChargeStation() {
        mRoutePackage.clearRouteItemByType(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.ROUTE_POINT_VIA_CHARGE_STATION);
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
        mSuccessMsg = successMsg;
        // 如果是预览状态，还是进入预览
        if (mNaviPackage.getPreviewStatus()) {
            OpenApiHelper.enterPreview(MapType.MAIN_SCREEN_MAIN_MAP);
        }
    }

    @Override
    public void onRouteItemClick(MapType mapTypeId, LayerPointItemType type, LayerItemRoutePointClickResult result) {
        if (Logger.openLog) {
            Logger.i(TAG, "onRouteItemClick result = ", result.toString(), " type = ", type);
        }
        if (ConvertUtils.isEmpty(result)) {
            Logger.e(TAG, "onRouteItemClick result is null");
            return;
        }
        long pathId;
        switch (type) {
            case ROUTE_POINT_VIA:
            case ROUTE_POINT_VIA_CHARGE:
                final PoiInfoEntity poiInfo = new PoiInfoEntity();
                poiInfo.setPoint(new GeoPoint(result.getLog(), result.getLat()));
                final Bundle poiBundle = new Bundle();
                poiBundle.putParcelable(
                        AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfo);
                poiBundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE,
                        AutoMapConstant.PoiType.POI_MAP_CLICK);
                boolean isNavi = StackManager.getInstance().
                        getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name()) instanceof
                        NaviGuidanceFragment;
                if (isNavi) {
                    poiBundle.putInt(NaviConstant.KEY_NO_HIDE_FRAGMENT, NumberUtils.NUM_1);
                }
                final PoiDetailsFragment poiFragment = new PoiDetailsFragment();
                addPoiDetailsFragment(poiFragment, poiBundle);
                hideNaviContent();
                break;
            case ROUTE_PATH:
                pathId = result.getIndex();
                onRouteClick(pathId);
                break;
            case ROUTE_GUIDE_LABEL:
                pathId = result.getEventID();
                onRouteClick(pathId);
                break;
            default:
                break;
        }
    }

    private void onRouteClick(long pathId) {
        int currentNaviType = mNaviPackage.getCurrentNaviType();
        if (currentNaviType != 0) {
            Logger.i(TAG, "非GPS 导航，不支持手动切换路线");
            return;
        }
        if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            Logger.i(TAG, "离线状态，不支持手动切换路线");
            return;
        }
        mNaviPackage.selectPath(MapType.MAIN_SCREEN_MAIN_MAP, pathId);
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
    public void onRoutePreferenceChange(RoutePreferenceID routePreferenceID) {
        Logger.i(TAG, "routePreferenceID: ", routePreferenceID);
        if (mViewModel != null) {
            mViewModel.setCurrentPreferences(getPreferences());
        }
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

    @Override
    public HashMap<NaviSceneId, Integer> getSceneStatus() {
        if (mViewModel != null) {
            return mViewModel.getSceneStatus();
        }
        return null;
    }

    @Override
    public void closeNavi() {
        Logger.i(TAG, "closeNavi");
        mViewModel.onNaviStop();
        mRoutePackage.removeAllRouteInfo(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    public void onNaviStart() {
        ThreadManager.getInstance().postUi(mInitLazyView);
    }

    @Override
    public void onPassByClick(final MapType mapType) {
        if (mapType == MapType.MAIN_SCREEN_MAIN_MAP) {
            if (mViewModel != null) {
                mViewModel.onPassByClick();
            }
        }
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
        if (Objects.equals(mapTypeId, MapType.MAIN_SCREEN_MAIN_MAP)) {
            switch (touchEvent.getAction()) {
                case MotionEvent.ACTION_DOWN:
                    mDownX = touchEvent.getX();
                    mDownY = touchEvent.getY();
                    mDownTime = System.currentTimeMillis();
                    break;
                case MotionEvent.ACTION_UP:
                    float upX = touchEvent.getX();
                    float upY = touchEvent.getY();
                    long upTime = System.currentTimeMillis();
                    if (Math.abs(upX - mDownX) < MOVE_THRESHOLD &&
                            Math.abs(upY - mDownY) < MOVE_THRESHOLD &&
                            (upTime - mDownTime) < CLICK_THRESHOLD) {
                        // 这是一次点击事件
                        if (mViewModel != null) {
                            mViewModel.onMapClick();
                        }
                    }
                    break;
                case MotionEvent.ACTION_MOVE:
                    float moveX = touchEvent.getX();
                    float moveY = touchEvent.getY();
                    float distance = (float) Math.hypot(moveX - mDownX, moveY - mDownY);
                    if (distance > MOVE_THRESHOLD) {
                        // 认为是滑动操作
                        if (mViewModel != null) {
                            mViewModel.onMapSwipe();
                        }
                    }
                default:
                    break;
            }
        }
    }

    @Override
    public boolean getIsViaArrived() {
        boolean isArrive = !TextUtils.isEmpty(mArrivedViaPoiId) &&
                Objects.equals(mArrivedViaPoiId, getNextViaPoiId());
        Logger.i(TAG, "getIsViaArrived isArrive = ", isArrive,
                " mArrivedViaPoiId = ", mArrivedViaPoiId, " mCurrentViaIndex = ", mCurrentViaIndex);
        return isArrive;
    }

    public void onDeletePath(ArrayList<Long> pathIDList) {
        if (!ConvertUtils.isEmpty(pathIDList)) {
            for (long pathId : pathIDList) {
                Logger.i(TAG, "onDeletePath pathId = ", pathId);
                mRoutePackage.removeRouteLineInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathId);
                OpenApiHelper.removePathById(pathId);
            }
            mNaviPackage.refreshPathList();
        }
    }

    /**
     * @return 下一个途经点的名称
     */
    private String getNextViaPoiId() {
        List<RouteParam> allPoiParamList = OpenApiHelper.getAllPoiParamList(
                MapType.MAIN_SCREEN_MAIN_MAP);
        if (!ConvertUtils.isEmpty(allPoiParamList) && allPoiParamList.size() > NumberUtils.NUM_2) {
            return allPoiParamList.get(NumberUtils.NUM_1).getPoiID();
        }
        return "";
    }

    @Override
    public void onRouteRequest() {
        if (!ConvertUtils.isNull(mViewModel)) {
            mViewModel.showProgressUI();
        }
    }

    @Override
    public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.hideProgressUI(true);
        }
        RouteLineInfo routeLineInfo = mRoutePackage.getSelectLineInfo(MapType.MAIN_SCREEN_MAIN_MAP);
        Logger.i(TAG, "getMDistance = ", (routeLineInfo == null ? "null" : routeLineInfo.getMDistance())
                , " NaviStatus:", mNaviSatusPackage.getCurrentNaviStatus());
        if (!ConvertUtils.isEmpty(routeLineInfo) && routeLineInfo.getMDistance() > ONE_ROUTE_LINE_DISTANCE) {
            mRoutePackage.showOnlyOneRouteLine(routeLineLayerParam.getMMapTypeId());
        } else {
            mRoutePackage.showRouteLine(routeLineLayerParam.getMMapTypeId());
        }
        if (!mIsAutoReRoute) {
            OpenApiHelper.enterPreview(MapType.MAIN_SCREEN_MAIN_MAP);
            ImmersiveStatusScene.getInstance().setImmersiveStatus(
                    MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        }
    }

    @Override
    public void onRouteFail(MapType mapTypeId, String errorMsg) {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.hideProgressUI(false);
        }
        if (!ConvertUtils.isEmpty(errorMsg)) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(errorMsg);
            });
        }
    }

    @Override
    public void onRouteOffline(MapType mapTypeId, String errorMsg) {
        if (!ConvertUtils.isEmpty(errorMsg)) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(errorMsg);
            });
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showOfflineProgressUI();
        }
    }

    @Override
    public void onReroute(FyRouteOption routeOption) {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            Logger.i(TAG, "偏航引发算路");
            mViewModel.showProgressUIOnly();
        }
    }

    @Override
    public void onReRouteError() {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            Logger.i(TAG, "静默算路失败");
            mViewModel.hideProgressUI(false);
        }
    }

    /***
     * 显示成功toast
     */
    public void showSuccessMsg() {
        if (!ConvertUtils.isEmpty(mSuccessMsg)) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(mSuccessMsg);
            });
        }
    }

    /**
     * 取消算路
     */
    public void cancelRoute() {
        mRoutePackage.abortRequest(MapType.MAIN_SCREEN_MAIN_MAP);
    }
}
