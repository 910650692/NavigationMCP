package com.sgm.navi.service.logicpaket.navi;

import android.graphics.Rect;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.guide.model.CrossImageInfo;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.cruise.CruiseAdapter;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.navi.GuidanceObserver;
import com.sgm.navi.service.adapter.navi.NaviAdapter;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.adapter.route.RouteAdapter;
import com.sgm.navi.service.adapter.setting.SettingAdapter;
import com.sgm.navi.service.adapter.signal.SignalAdapter;
import com.sgm.navi.service.adapter.signal.SignalAdapterCallback;
import com.sgm.navi.service.adapter.speech.SpeechAdapter;
import com.sgm.navi.service.adapter.user.msgpush.MsgPushAdapter;
import com.sgm.navi.service.adapter.user.msgpush.MsgPushAdapterCallback;
import com.sgm.navi.service.adapter.user.usertrack.UserTrackAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.CameraInfoEntity;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.FyElecVehicleETAInfo;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviCongestionInfoEntity;
import com.sgm.navi.service.define.navi.NaviDriveReportEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviExchangeEntity;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviModelSaveEntity;
import com.sgm.navi.service.define.navi.NaviRoadFacilityEntity;
import com.sgm.navi.service.define.navi.NaviStartType;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.navi.RoadName;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.navi.SoundInfoEntity;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navi.SuggestChangePathReasonEntity;
import com.sgm.navi.service.define.navi.TrafficLightCountdownEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.RouteCurrentPathParam;
import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.route.RouteSpeechRequestParam;
import com.sgm.navi.service.define.search.ETAInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.user.msgpush.MsgPushInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.sgm.navi.service.define.user.usertrack.HistoryPoiItemBean;
import com.sgm.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.greendao.history.HistoryManager;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.service.tts.NaviMediaPlayer;
import com.sgm.navi.service.tts.TTSPlayHelper;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * @author sgm
 * @version $Revision.*$
 */
public final class NaviPackage implements GuidanceObserver, SignalAdapterCallback, StartService.ISdkInitCallback, MsgPushAdapterCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    @Getter
    private CrossImageEntity lastCrossEntity;
    @Getter
    private boolean mCrossImgIsOnShowing = false;
    private NaviAdapter mNaviAdapter;
    private LayerAdapter mLayerAdapter;
    private MapPackage mapPackage;
    private SpeechAdapter mSpeechAdapter;
    private final ConcurrentHashMap<String, IGuidanceObserver> mGuidanceObservers;
    private NavistatusAdapter mNavistatusAdapter;
    private final HistoryManager mManager;
    private RouteAdapter mRouteAdapter;
    private UserTrackAdapter mUserTrackAdapter;
    private SignalAdapter mSignalAdapter;
    private boolean mIsMute = false;
    private int mCurrentImmersiveStatus = -1;
    private boolean mIsPreview = false;
    @Getter
    private NaviEtaInfo mCurrentNaviEtaInfo;
    private boolean mIsFixedOverView;
    private boolean mIsClusterFixOverView;
    private boolean mCruiseVoiceIsOpen = true; // 巡航播报是否开启
    // 线路上所有道路的名称
    private RoadName mRoadName;
    /**
     * 当前导航类型 -1:未知 0:GPS导航 1:模拟导航
     */
    private int mCurrentNaviType = NumberUtils.NUM_ERROR;
    // 缓存初次进入导航的系统导航音量值，用来做恢复操作
    private int mLastSystemNaviVolume = NumberUtils.NUM_ERROR;
    private List<OnPreViewStatusChangeListener> mOnPreViewStatusChangeListeners =
            new CopyOnWriteArrayList<>();

    private final NaviModelSaveEntity mModelSaveEntity;
    private boolean mIsShowLanes;
    // 终点扎标显示内容
    private LayerItemRouteEndPoint mEndPoint;

    private AppFocusHelper mAppFocusHelper;
    @Getter
    private boolean hasSetCrossRect = false;
    @Setter
    private boolean mIsNaviViewActive = false; // 记录引导页面是否存在
    @Getter
    @Setter
    private NaviDriveReportEntity mNaviDriveReportEntity;
    @Getter
    private String mArriveTime = "";
    @Getter
    @Setter
    private boolean mIsFloatWindowShow;
    private NaviPackage() {
        StartService.getInstance().registerSdkCallback(TAG, this);
        mGuidanceObservers = new ConcurrentHashMap<>();
        mManager = HistoryManager.getInstance();
        mManager.init();
        mModelSaveEntity = new NaviModelSaveEntity();
    }

    @Override
    public void onSdkInitSuccess() {
        int muteStatus = SettingAdapter.getInstance().getConfigKeyMute();
        if (muteStatus == NumberUtils.NUM_1) {
            SettingAdapter.getInstance().setConfigKeyMute(NumberUtils.NUM_0);
        }
        StartService.getInstance().unregisterSdkCallback(TAG, this);
        if (DeviceUtils.isCar(AppCache.getInstance().getMContext())) {
            Logger.d("AppFocusHelper", "汽车环境，开启导航互斥");
            mAppFocusHelper = AppFocusHelper.getInstance();
            mAppFocusHelper.init(AppCache.getInstance().getMContext());
        }
    }

    public void releaseAppFocus() {
        if (mAppFocusHelper != null) {
            mAppFocusHelper.release();
        }
    }

    /**
     * init引导服务
     */
    public void initNaviService() {
        mSpeechAdapter = SpeechAdapter.getInstance();
        mNaviAdapter = NaviAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mapPackage = MapPackage.getInstance();
        mRouteAdapter = RouteAdapter.getInstance();
        mUserTrackAdapter = UserTrackAdapter.getInstance();
        mSignalAdapter = SignalAdapter.getInstance();
        mNaviAdapter.initNaviService();
        mNaviAdapter.registerObserver("NaviPackage", this);
        mSignalAdapter.registerCallback("NaviPackage", this);
        MsgPushAdapter.getInstance().registerCallBack(TAG, this);
    }

    /**
     * unInit引导服务
     */
    public void unInitNaviService() {
        if (mNaviAdapter != null) {
            mNaviAdapter.unInitNaviService();
        }
        releaseAppFocus();
    }

    /**
     * 开始导航
     *
     * @param isSimulate 是否是模拟导航
     * @return 是否导航成功
     */
    public boolean startNavigation(final boolean isSimulate) {
        if (mNaviAdapter == null) {
            Logger.e(TAG, "startNavigation", "mNaviAdapter == null");
            return false;
        }
        // bug: 1093377 如果未退出巡航态的情况下开启导航会开启导航失败，所以在开启导航前先判断下是否处于巡航状态，如果处于巡航态需要先退出
        String currentNaviStatus = NavistatusAdapter.getInstance().getCurrentNaviStatus();
        Logger.i(TAG, "startNavigation: currentNaviStatus = " + currentNaviStatus);
        if (NaviStatus.NaviStatusType.CRUISE.equals(currentNaviStatus)) {
            CruiseAdapter.getInstance().stopCruise();
        }
        final boolean result = mNaviAdapter.startNavigation(isSimulate ?
                NaviStartType.NAVI_TYPE_SIMULATION : NaviStartType.NAVI_TYPE_GPS);
        if (result) {
            mNaviDriveReportEntity = null;
            if (isSimulate) {
                mCurrentNaviType = NumberUtils.NUM_1;
                mNaviAdapter.setSimulationSpeed(40);
            } else {
                mCurrentNaviType = NumberUtils.NUM_0;
            }
            mLayerAdapter.setVisibleGuideSignalLight(MapType.MAIN_SCREEN_MAIN_MAP, true);
            mapPackage.setMapLabelClickable(MapType.MAIN_SCREEN_MAIN_MAP, false);
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NAVING);
            //清除终点停车场扎标
            mLayerAdapter.updateRouteEndPointParkViewVisible(MapType.MAIN_SCREEN_MAIN_MAP, false);
            mLayerAdapter.setCarLogoVisible(MapType.MAIN_SCREEN_MAIN_MAP, true);
            if (mAppFocusHelper != null) {
                mAppFocusHelper.startCarMapNavigation();
            }
        } else {
            Logger.e(TAG, "###Fatal### startNavi fail");
            mCurrentNaviType = NumberUtils.NUM_ERROR;
        }
        mRouteAdapter.sendL2Data(MapType.MAIN_SCREEN_MAIN_MAP);
        return result;
    }

    /**
     * isManual 是否手动结束导航
     * @return 是否停止导航成功
     */
    public boolean stopNavigation(boolean isManual) {
        if (mNaviAdapter == null) {
            Logger.e(TAG, "stopNavigation", "mNaviAdapter == null");
            return false;
        }
        final boolean result = mNaviAdapter.stopNavigation();
        if (result) {
            mLayerAdapter.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
            mapPackage.setMapLabelClickable(MapType.MAIN_SCREEN_MAIN_MAP, true);
            mCurrentNaviType = NumberUtils.NUM_ERROR;
            if(isManual) {
                sendBuryPointForCloseNavi();
            }
            if (mAppFocusHelper != null){
                mAppFocusHelper.stopCarMapNavigation();
            }
        }
        return result;
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_END_MANUAL)
    private void sendBuryPointForCloseNavi() {
        NaviEtaInfo mNaviEtaInfo = getCurrentNaviEtaInfo();
        if (mNaviEtaInfo == null) {
            Logger.e(MapDefaultFinalTag.NAVI_BURY_POINT, "eventName-" + BuryConstant.EventName.AMAP_NAVI_END_MANUAL + ": mNaviEtaInfo is null");
            return;
        }
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_REMAINING_TIME, TimeUtils.getRemainingTime(AppCache.getInstance().getMContext(), mNaviEtaInfo.getRemainTime()))
                .setParams(BuryConstant.ProperType.BURY_KEY_TRIP_DISTANCE, TimeUtils.getRemainingMileage(AppCache.getInstance().getMContext(), mNaviEtaInfo.getRemainDist()))
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * 注册GuidanceObserver回调，HMI层触发
     *
     * @param key              key
     * @param guidanceObserver GuidanceObserver
     */
    public void registerObserver(final String key, final IGuidanceObserver guidanceObserver) {
        mGuidanceObservers.put(key, guidanceObserver);
    }

    @Override
    public void onSystemStateChanged(int state) {
        Logger.i(TAG, "state：" + state);
        if (state == 4) {//4是熄火状态
            if (null != mCurrentNaviEtaInfo &&
                    mCurrentNaviEtaInfo.getRemainDist() < 1000) {//添加未完成导航(车辆熄火前的CVP距离目的地的距离 ≥ 1 km)
                Logger.i(TAG, "isCompleted= ");
                return;
            }
            addNaviRecord(true);
        }
    }

    /**
     * 移除GuidanceObserver回调，HMI层触发
     *
     * @param key key
     */
    public void unregisterObserver(final String key) {
        mGuidanceObservers.remove(key);
    }

    /**
     * 仪表地图触发全览
     */
    public void onMeterAction() {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onMeterAction();
                    }
                }
            }
        });
    }

    /***
     * 添加导航记录
     * 非手动结束，到达目的地结束，下电结束之外的异常结束都会被认为是未完成导航
     * @param isCompleted 是否完成导航 由于逻辑调整，现对此做出解释，默认插入时都为true，只有在接收下电信号时如果距离大于1KM则为false
     **/
    public void addNaviRecord(final boolean isCompleted) {
        ThreadManager.getInstance().execute(() -> {
            if (mRouteAdapter == null) {
                Logger.w(TAG, "addNaviRecord", "mRouteAdapter == null");
                return;
            }
            if (mUserTrackAdapter == null) {
                Logger.w(TAG, "addNaviRecord", "mUserTrackAdapter == null");
                return;
            }
            if (mManager == null) {
                Logger.w(TAG, "addNaviRecord", "mManager == null");
                return;
            }
            final List<RouteParam> allPoiParamList = mRouteAdapter.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
            final boolean isEmpty = ConvertUtils.isEmpty(allPoiParamList);
            Logger.i(TAG, "isCompleted= " + isCompleted + ",isEmpty：" + isEmpty);
            if (!isEmpty) {
                final RouteParam startParam = allPoiParamList.get(0);
                final RouteParam endParam = allPoiParamList.get(allPoiParamList.size() - 1);
                RouteSpeechRequestParam routeSpeechRequestParam = new RouteSpeechRequestParam();
                final History history = new History();
                String key = endParam.getName();
                history.setMKeyWord(key);
                String endPoiId = endParam.getPoiID();
                history.setMPoiId(endPoiId);
                String startName = startParam.getName();
                history.setMStartPoiName(startName);
                String endName = endParam.getName();
                history.setMEndPoiName(endName);
                GeoPoint startPoint = startParam.getRealPos();
                history.setMStartPoint(startPoint.toString());
                GeoPoint endPoint = endParam.getRealPos();
                history.setMEndPoint(endPoint.toString());
                history.setMType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY);
                history.setMIsCompleted(isCompleted);
                Date date = new Date();
                history.setMUpdateTime(date);
                history.setMAddress(endParam.getAddress());
                List<PoiInfoEntity> midPoint = new ArrayList<>();
                // [0]代表起点 [size-1]代表终点
                for (int i = 0; i < allPoiParamList.size(); i++) {
                    RouteParam routeParam = allPoiParamList.get(i);
                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                    poiInfoEntity.setName(routeParam.getName());
                    poiInfoEntity.setAddress(routeParam.getAddress());
                    poiInfoEntity.setPid(routeParam.getPoiID());
                    poiInfoEntity.setPoint(routeParam.getRealPos());
                    if (i == 0) {
                    } else if (i == allPoiParamList.size() - 1) {
                        poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                        routeSpeechRequestParam.setMEndPoiInfoEntity(poiInfoEntity);
                    } else {
                        poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_WAY);
                        midPoint.add(poiInfoEntity);
                    }
                }
                routeSpeechRequestParam.setMViaPoiInfoEntityList(midPoint);
                history.setMViaPoint(GsonUtils.toJson(routeSpeechRequestParam));
                Logger.i(TAG, "addNaviRecord history name:" + history.getMEndPoiName());
                mManager.insertOrReplace(history);
                // 如果登陆了话就将数据传给高德
                if (AccountPackage.getInstance().isLogin()) {
                    HistoryRouteItemBean historyRouteItemBean = new HistoryRouteItemBean();
//                historyRouteItemBean.setId(key);
                    historyRouteItemBean.setStartLoc(startPoint);
                    historyRouteItemBean.setEndLoc(endPoint);
                    historyRouteItemBean.setIsCompleted(isCompleted);
                    historyRouteItemBean.setUpdateTime(date.getTime());
                    historyRouteItemBean.setType(
                            AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY);
                    HistoryPoiItemBean startPoiItemBean = new HistoryPoiItemBean();
                    startPoiItemBean.setPoiId(startParam.getPoiID());
                    startPoiItemBean.setName(startName);
                    historyRouteItemBean.setFromPoi(startPoiItemBean);
                    HistoryPoiItemBean endPoiItemBean = new HistoryPoiItemBean();
                    endPoiItemBean.setPoiId(endPoiId);
                    endPoiItemBean.setName(endName);
                    endPoiItemBean.setAddress(endParam.getAddress());
                    historyRouteItemBean.setToPoi(endPoiItemBean);
                    mUserTrackAdapter.addHistoryRoute(historyRouteItemBean);
                }
            } else if (isCompleted) {
                // 下电后不再弹出继续导航
                mManager.updateUncompletedNavi();
            }
        });
    }

    public static NaviPackage getInstance() {
        return Helper.NAVI_PACKAGE;
    }

    /**
     * @param b 是否固定视图状态 @thread main
     */
    public void setFixedOverViewStatus(final boolean b) {
        mIsFixedOverView = b;
    }

    /**
     * @return 是否固定视图状态 @thread main
     */
    public boolean getFixedOverViewStatus() {
        return mIsFixedOverView;
    }

    /**
     * 由于cluster导航视图打开后，需要固定视图状态，所以需要单独设置
     * @param b 是否固定视图状态 @thread main
     */
    public void setClusterFixOverViewStatus(final boolean b) {
        mIsClusterFixOverView = b;
    }

    public boolean getClusterFixOverViewStatus() {
        return mIsClusterFixOverView;
    }

    public void restoreNavigationByRebuild() {
        Logger.i(TAG, "restoreNavigationByRebuild");
        if (ConvertUtils.isEmpty(mModelSaveEntity)) {
            Logger.e(TAG, "restoreNavigationByRebuild", "mModelSaveEntity is null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onNaviInfo(mModelSaveEntity.getNaviEtaInfo());
                        guidanceObserver.onManeuverInfo(mModelSaveEntity.getNaviManeuverInfo());
                        guidanceObserver.onUpdateTMCLightBar(mModelSaveEntity.getNaviTmcInfo());
                        guidanceObserver.onNaviSpeedOverallInfo(mModelSaveEntity.
                                getSpeedOverallEntity());
                        guidanceObserver.onNaviSAPAInfo(mModelSaveEntity.getSapaInfoEntity());
                        guidanceObserver.onLaneInfo(mIsShowLanes, mModelSaveEntity.getLaneInfo());
                        guidanceObserver.onUpdateElectVehicleETAInfo(mModelSaveEntity.
                                getFyElecVehicleETAInfos());
                    }
                }
            }
        });
    }

    public void refreshPathList() {
        long currentPathID = OpenApiHelper.getCurrentPathId(MapType.MAIN_SCREEN_MAIN_MAP);
        ArrayList<PathInfo> pathInfos = new ArrayList<>(OpenApiHelper.getCurrentPathInfos());
        int mainPathIndex = NumberUtils.NUM_ERROR;
        if (pathInfos.isEmpty()) {
            Logger.i(TAG, "refreshPathList: pathInfos is empty");
            return;
        }
        for (int i = 0; i < pathInfos.size(); i++) {
            if (pathInfos.get(i).getPathID() == currentPathID) {
                mainPathIndex = i;
                break;
            }
        }
        if (mainPathIndex != NumberUtils.NUM_ERROR) {
            mNaviAdapter.updatePathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathInfos, mainPathIndex);
        }
    }

    @Override
    public void initService() {

    }

    @Override
    public void notifyAutoPushMessage(MsgPushInfo msg) {

    }

    @Override
    public void notifyAimPoiPushMessage(MsgPushInfo msg) {

    }

    @Override
    public void notifyAimRoutePushMessage(RouteMsgPushInfo routeMsgPushInfo) {

    }

    @Override
    public void notifyPlanPrefPushMessage(ArrayList<Integer> planPrefs) {

    }

    @Override
    public void notifyQuitNaviPushMessage() {
        Logger.i(TAG, "notifyQuitNaviPushMessage");
        // 因为退出后台页面可能会被kill，所以如果页面在通过页面关闭导航，如果页面不存在了直接关闭导航
        if (mIsNaviViewActive) {
            ThreadManager.getInstance().postUi(() -> {
                if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                    for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                        if (guidanceObserver != null) {
                            guidanceObserver.quitByMessage();
                        }
                    }
                }
            });
        } else {
            stopNavigation(true);
        }

    }

    @Override
    public void notifyMobileLinkPushMessage(MsgPushInfo msg) {

    }

    @Override
    public void onRecvAckGSendToPhoneResponse(MsgPushResponseInfo sendToPhoneResponseParam) {

    }

    @Override
    public void onRecvAckGWsTserviceInternalLinkAutoReportResponse(MsgPushResponseInfo gwsTserviceInternalLinkAutoReportResponseParam) {

    }

    private static final class Helper {
        private static final NaviPackage NAVI_PACKAGE = new NaviPackage();
    }

    /**
     * @param pathID 换为主路线的id @thread main
     */
    public void selectMainPathID(final long pathID) {
        ThreadManager.getInstance().postUi(() -> {
            if (mNaviAdapter != null) {
                mNaviAdapter.selectMainPathID(pathID);
            }
        });
    }

    /**
     * 设置选择的主路线
     *
     * @param mapTypeId 屏幕id
     * @param pathId    路线id
     */
    public void selectPath(final MapType mapTypeId, final long pathId) {
        Logger.i(TAG, "selectPath: ", pathId, "mapId = ", mapTypeId);
        selectMainPathID(pathId);
    }

    /*设置静音*/
    public void setMute(final boolean isMute) {
        SettingAdapter.getInstance().setConfigKeyMute(isMute ? 1 : 0);
        mIsMute = isMute;
        if (isMute) {
            setCurrentNaviVolume(mSignalAdapter.getNaviVolume());
            mSignalAdapter.setNaviVolume(NumberUtils.NUM_0);
        } else {
            if (mLastSystemNaviVolume > NumberUtils.NUM_0) {
                mSignalAdapter.setNaviVolume(mLastSystemNaviVolume);
            }
        }
    }

    public void setMuteByHmi(final boolean isMute) {
        SettingAdapter.getInstance().setConfigKeyMute(isMute ? 1 : 0);
        mIsMute = isMute;
    }

    public void setCurrentNaviVolume(int volume) {
        mLastSystemNaviVolume = volume;
    }

    public int getCurrentNaviVolume() {
        return mLastSystemNaviVolume;
    }

    /*是否静音*/
    public boolean isMute() {
        Logger.i(TAG, "isMute: " + mIsMute);
        return mIsMute;
    }

    /**
     * 获取服务区和收费站信息
     *
     * @param isFindRemainPath 是否查询剩余路线  @thread main
     */
    public void obtainSAPAInfo(final boolean isFindRemainPath) {
        ThreadManager.getInstance().postUi(() -> {
            if (mNaviAdapter != null) {
                final long l = mNaviAdapter.obtainSAPAInfo(isFindRemainPath);
                Logger.i(TAG, "obtainSAPAInfo: " + l);
            }
        });
    }

    @Override
    public void onNaviStart() {
        Logger.i(TAG, "onNaviStar 导航开启 ");
        ThreadManager.getInstance().postUi(() -> {
            if (ConvertUtils.isEmpty(mGuidanceObservers)) return;
            for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver == null) return;
                guidanceObserver.onNaviStart();
            }
        });
    }

    @Override
    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        Logger.i(TAG, "onNaviInfo: ");
        mCurrentNaviEtaInfo = naviETAInfo;
        if (mCurrentNaviEtaInfo != null) {
            int time = mCurrentNaviEtaInfo.getRemainTime();
            mArriveTime = TimeUtils.getArriveTime(AppCache.getInstance().getMContext(), time);
            Logger.i(TAG, mArriveTime);
        }
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        mModelSaveEntity.setNaviEtaInfo(naviETAInfo);
                        guidanceObserver.onNaviInfo(naviETAInfo);
                    }
                }
            }
        });
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
        Logger.i(TAG, "onLaneInfo: " + isShowLane);
        mIsShowLanes = isShowLane;
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        mModelSaveEntity.setLaneInfo(laneInfoEntity);
                        guidanceObserver.onLaneInfo(isShowLane, laneInfoEntity);
                    }
                }
            }
        });
    }

    @Override
    public void onCrossImageInfo(final boolean isShowImage,
                                 final CrossImageEntity naviImageInfo) {
        Logger.i(TAG, "onCrossImageInfo isShowImage:" + isShowImage + " naviImageInfo:" + naviImageInfo);
        mapPackage.openOrCloseScreenshot(MapType.MAIN_SCREEN_MAIN_MAP, isShowImage);
        lastCrossEntity = naviImageInfo;
        mCrossImgIsOnShowing = isShowImage;
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onCrossImageInfo(isShowImage, naviImageInfo);
                    }
                }
            }
        });
    }

    @Override
    public void onShowCrossImage(CrossImageInfo info) {
        mLayerAdapter.setCrossImageInfo(MapType.MAIN_SCREEN_MAIN_MAP, info);
    }

    @Override
    public void onNaviArrive(final long traceId, final int naviType) {
        Logger.i(TAG, "onNaviArrive: traceId = " + traceId + ", naviType = " + naviType);
        stopNavigation(false);
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onNaviArrive(traceId, naviType);
                    }
                }
            }
        });
    }

    @Override
    public void onNaviStop() {
        Logger.i(TAG, "onNaviStop");
        ThreadManager.getInstance().postUi(() -> {
            closeNavi();
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onNaviStop();
                    }
                }
            }
        });
        mManager.updateUncompletedNavi();
        hasSetCrossRect = false;
    }

    @Override
    public void onPlayTTS(final SoundInfoEntity info) {
        if (info == null) {
            return;
        }
        final boolean nopOpen = SignalPackage.getInstance().getNavigationOnAdasTextToSpeachStatus();
        Logger.i(TAG, "onPlayTTS: ", info.getText(), " rangeType:", info.getRangeType(), " isHighPriority:", info.isHighPriority(),
                " isRingType:", info.isRingType(), " NOP:", nopOpen, " 导航状态:", NaviStatusPackage.getInstance().getCurrentNaviStatus(),
                " 巡航开关:", mCruiseVoiceIsOpen, " 静音:", mIsMute);
        ThreadManager.getInstance().postUi(() -> {
            try {
                if (NaviAdapter.getInstance().getMCountDownLatch() != null) {
                    NaviAdapter.getInstance().setSoundInfoEntity(info);
                    NaviAdapter.getInstance().getMCountDownLatch().countDown();
                }
                // 如果处于巡航态且巡航播报关闭
                if (NavistatusAdapter.getInstance().getCurrentNaviStatus() == NaviStatus.NaviStatusType.CRUISE && !mCruiseVoiceIsOpen) {
                    Logger.d(TAG, "当前处于巡航态且播报关闭，故不播放声音！");
                    return;
                }
                if (mIsMute) {
                    return; // 静音开关
                }
                //如果NOP打开并且是不播报的类型则不播报  soundType废弃 换成rangeType
                if (nopOpen && !TTSPlayHelper.allowToPlayWithNopOpen(info.getRangeType())) {
                    Logger.d(TAG, "此类型下不支持播报");
                    return;
                }
                mSpeechAdapter.synthesize(!info.isHighPriority(), info.getText());
            } catch (Exception e) {
                Logger.e(TAG, "playTTs exception:" + e.getMessage());
            }
        });
    }

    @Override
    public void onPlayRing(int type) {
        // 如果处于巡航态且巡航播报关闭
        if (Objects.equals(NavistatusAdapter.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.CRUISE) && !mCruiseVoiceIsOpen) {
            Logger.d(TAG, "onPlayRing 当前处于巡航态且播报关闭，故不播放声音！");
            return;
        }
        if (mIsMute) {
            Logger.d(TAG, "onPlayRing 已静音 type:", type);
            return; // 静音开关
        }
        if (TextUtils.equals("cadi", BuildConfig.FLAVOR)) {
            //TODO 凯迪车型MediaPlayer导致ANR，待FW解决后放开
            Logger.d(TAG, "onPlayRing 凯迪车型MediaPlayer导致ANR，待FW解决后放开");
            return;
        }
        ThreadManager.getInstance().postUi(() -> NaviMediaPlayer.getInstance().playNaviWarningSound(type));
    }

    @Override
    public void onManeuverInfo(final NaviManeuverInfo respData) {
        Logger.i(TAG, "onManeuverInfo");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        mModelSaveEntity.setNaviManeuverInfo(respData);
                        guidanceObserver.onManeuverInfo(respData);
                    }
                }
            }
        });
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        Logger.i(TAG, "onUpdateTMCLightBar");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        mModelSaveEntity.setNaviTmcInfo(naviTmcInfo);
                        guidanceObserver.onUpdateTMCLightBar(naviTmcInfo);
                    }
                }
            }
        });
    }

    @Override
    public void onNaviSpeedOverallInfo(final SpeedOverallEntity speedEntity) {
        Logger.i(TAG, "onNaviSpeedOverallInfo");
        if (!ConvertUtils.isEmpty(speedEntity)) {
            if (speedEntity.getSpeedType() == NaviConstant.SpeedType.SPEED_GREEN_WAVE) {
                boolean netStatus = Boolean.TRUE.equals(
                        NetWorkUtils.Companion.getInstance().checkNetwork());
                // 1036953 模拟导航和离线状态不支持绿波车速
                if (mCurrentNaviType == NumberUtils.NUM_1 || !netStatus) {
                    return;
                }
            }
            ThreadManager.getInstance().postUi(() -> {
                if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                    for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                        if (guidanceObserver != null) {
                            mModelSaveEntity.setSpeedOverallEntity(speedEntity);
                            guidanceObserver.onNaviSpeedOverallInfo(speedEntity);
                        }
                    }
                }
            });
        }
    }

    @Override
    public void onNaviCameraInfo(final CameraInfoEntity cameraInfo) {
        Logger.i(TAG, "onNaviCameraInfo");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onNaviCameraInfo(cameraInfo);
                    }
                }
            }
        });
    }

    @Override
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        if (sapaInfoEntity == null) {
            Logger.i(TAG, "onNaviSAPAInfo sapaInfoEntity is null");
            return;
        }
        //Logger.i(TAG, "onNaviSAPAInfo entity = " + sapaInfoEntity.toString());
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        mModelSaveEntity.setSapaInfoEntity(sapaInfoEntity);
                        guidanceObserver.onNaviSAPAInfo(sapaInfoEntity);
                    }
                }
            }
        });
    }

    @Override
    public void onUpdateViaPass(final long viaIndex) {
        Logger.i(TAG, "onUpdateViaPass");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onUpdateViaPass(viaIndex);
                    }
                }
            }
        });
    }

    @Override
    public void onBatterHotCallBack(final boolean isPass) {
        List<RouteParam> params = RoutePackage.getInstance().getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (params.size() <= 1) {
            return;
        }
        int powerLevel = 0;
        int status = 1;
        final int[] timeToArrival = {0};
        int distToArrival = 0;
        PoiInfoEntity info = params.get(1).getMPoiInfoEntity();
        if (Boolean.TRUE.equals(isPass)) {
            status = 3;
        }
        if (!ConvertUtils.isEmpty(info) && info.getPointTypeCode() != null
                && AutoMapConstant.PointTypeCode.CHARGING_STATION == SearchPackage.getInstance().getPointTypeCode(info.getPointTypeCode())) {
            status = 2;
            if (!ConvertUtils.isEmpty(info.getChargeInfoList())
                    && !ConvertUtils.isEmpty(info.getChargeInfoList().get(0))) {
                powerLevel = info.getChargeInfoList().get(0).getSlowPower();
            }
            CompletableFuture<ETAInfo> completableFuture = SearchPackage.getInstance().getTravelTimeFutureIncludeChargeLeft(info.getPoint());
            int finalStatus = status;
            int finalPowerLevel = powerLevel;
            completableFuture.thenAccept(etaInfo -> {
                if (etaInfo.getTime() > 2047 * 60) {
                    timeToArrival[0] = 2047;
                } else {
                    timeToArrival[0] = (int) (etaInfo.getTime() / 60);
                }
                SignalPackage.getInstance().setNextChargingDestination(finalPowerLevel, finalStatus, timeToArrival[0], distToArrival);
            }).exceptionally(throwable -> {
                SignalPackage.getInstance().setNextChargingDestination(finalPowerLevel, finalStatus, timeToArrival[0], distToArrival);
                return null;
            });
        } else {
            SignalPackage.getInstance().setNextChargingDestination(powerLevel, status, timeToArrival[0], distToArrival);
        }

        if (Boolean.TRUE.equals(isPass)) {
            status = 1;
            SignalPackage.getInstance().setNextChargingDestination(powerLevel, status, timeToArrival[0], distToArrival);
        }
    }

    @Override
    public void onSelectMainPathStatus(final long pathID, final int result) {
        Logger.i(TAG, "onSelectMainPathStatus " + pathID + " result " + result);
        if (result == NaviConstant.ChangeNaviPathResult.CHANGE_NAVI_PATH_RESULT_SUCCESS) {
            PathInfo pathInfo = OpenApiHelper.getPathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathID);
            OpenApiHelper.setCurrentPathInfo(pathInfo);
            RouteCurrentPathParam routeCurrentPathParam = new RouteCurrentPathParam();
            routeCurrentPathParam.setMPathInfo(pathInfo);
            RouteAdapter.getInstance().sendL2Data(routeCurrentPathParam);
        }
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onSelectMainPathStatus(pathID, result);
                    }
                }
            }
        });
    }

    @Override
    public void updateBroadcastParam(final int broadcastType, final boolean isDay) {
        Logger.i(TAG, " broadcastType = " + broadcastType + ", isDay = " + isDay);
        if (mNaviAdapter == null) {
            mNaviAdapter = NaviAdapter.getInstance();
        }
        mNaviAdapter.updateBroadcastParam(broadcastType, isDay);
    }

    @Override
    public void onCurrentRoadSpeed(final int speed) {
        Logger.i(TAG, "onCurrentRoadSpeed");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onCurrentRoadSpeed(speed);
                    }
                }
            }
        });
    }

    @Override
    public void onUpdateChargeStationPass(long viaIndex) {
        GuidanceObserver.super.onUpdateChargeStationPass(viaIndex);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateChargeStationPass(viaIndex);
                }
            }
        }
    }

    @Override
    public void onUpdateElectVehicleETAInfo(final List<FyElecVehicleETAInfo> infos) {
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                    for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                        if (guidanceObserver != null) {
                            guidanceObserver.onUpdateElectVehicleETAInfo(infos);
                            if (mModelSaveEntity != null) {
                                mModelSaveEntity.setFyElecVehicleETAInfos(infos);
                            }
                        }
                    }
                }
            }
        });
    }

    @Override
    public void onUpdateTrafficLightCountdown(
            final ArrayList<TrafficLightCountdownEntity> list) {
        Logger.i(TAG, "onUpdateTrafficLightCountdown: size = " + list.size());
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateTrafficLightCountdown(list);
                }
            }
        }
    }

    @Override
    public void onDeletePath(ArrayList<Long> pathIDList) {
        if (ConvertUtils.isEmpty(pathIDList)) {
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onDeletePath(pathIDList);
                    }
                }
            }
        });
    }

    @Override
    public void onChangeNaviPath(long oldPathId, long pathID) {
        Logger.i(TAG, "onChangeNaviPath oldPathId = " + oldPathId + " pathID = " + pathID);
        PathInfo pathInfo = OpenApiHelper.getPathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathID);
        OpenApiHelper.setCurrentPathInfo(pathInfo);
        RouteCurrentPathParam routeCurrentPathParam = new RouteCurrentPathParam();
        routeCurrentPathParam.setMPathInfo(pathInfo);
        RouteAdapter.getInstance().sendL2Data(routeCurrentPathParam);
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onChangeNaviPath(oldPathId, pathID);
                    }
                }
            }
        });
    }

    @Override
    public void onSuggestChangePath(long newPathID,
                                    long oldPathID, SuggestChangePathReasonEntity reason) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onSuggestChangePath(newPathID, oldPathID, reason);
                    }
                }
            }
        });
    }

    @Override
    public void onShowNaviFacility(ArrayList<NaviRoadFacilityEntity> naviRoadFacilityEntity) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onShowNaviFacility(naviRoadFacilityEntity);
                    }
                }
            }
        });
    }

    @Override
    public void onUpdateTMCCongestionInfo(NaviCongestionInfoEntity naviCongestionInfoEntity) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onUpdateTMCCongestionInfo(naviCongestionInfoEntity);
                    }
                }
            }
        });
    }

    @Override
    public void onDriveReport(NaviDriveReportEntity report) {
        List<RouteParam> routeParams = mRouteAdapter.
                getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (mNaviDriveReportEntity == null) {
            mNaviDriveReportEntity = new NaviDriveReportEntity();
        }
        if (!ConvertUtils.isEmpty(routeParams)) {
            mNaviDriveReportEntity.setEndPos(routeParams.get(routeParams.size() - 1).getName());
        }
        int predictedFuelSavingPer100km = mSignalAdapter.getPredictedFuelSavingPer100km();
        if(predictedFuelSavingPer100km < 0){
            predictedFuelSavingPer100km = 0;
        }
        mNaviDriveReportEntity.setEstimateFuelConsume(predictedFuelSavingPer100km);
        int totalFuelSaving = mSignalAdapter.getTotalFuelSaving();
        if(totalFuelSaving < 0){
            totalFuelSaving = 0;
        }
        mNaviDriveReportEntity.setTotalFuelConsume(totalFuelSaving);
        mNaviDriveReportEntity.setDrivenDist(report.getDrivenDist());
        mNaviDriveReportEntity.setDrivenTime(report.getDrivenTime());
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onDriveReport(mNaviDriveReportEntity);
                    }
                }
            }
        });
    }

    /**
     * @param surfaceViewId 屏幕id.
     * @param rect          路口矩形区域.
     */
    public void setRoadCrossRect(final MapType surfaceViewId, final Rect rect) {
        Logger.d(TAG, "setRoadCrossRect", rect.toShortString());
        mLayerAdapter.updateRoadCrossRect(surfaceViewId, rect);
        mapPackage.updateScreenshotRect(surfaceViewId, rect);
        hasSetCrossRect = true;
    }

    /**
     * 语音打开/关闭引导中路线全览.
     *
     * @param mapTypeId 屏幕id.
     * @param open      true-开启全览  false-关闭全览.
     */
    public void voiceRouteOverview(final MapType mapTypeId, final boolean open) {
        Logger.i(TAG, "voiceRouteOverview: " + open);
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onVoiceOverview(mapTypeId, open);
                    }
                }
            }
        });
    }

    /**
     * 语音切换主辅路、桥上下.
     *
     * @param mapTypeId      MapTypeId，对应底图.
     * @param parallelOption 切换类型，MAIN-主路 SIDE-辅路  ON-桥上  UNDER-桥下.
     */
    public void voiceChangeParallelRoad(final MapType mapTypeId, final String parallelOption) {
        Logger.i(TAG, "voiceChangeParallelRoad: " + parallelOption);
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onVoiceParallelOption(mapTypeId, parallelOption);
                    }
                }
            }
        });
    }

    /**
     * 语音继续导航指令.
     *
     * @param mapTypeId MapTypeId，对应底图.
     */
    public void voiceContinueNavigation(final MapType mapTypeId) {
        Logger.i(TAG, "voiceContinueNavigation");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onVoiceContinueNavigation(mapTypeId);
                    }
                }
            }
        });
    }

    public interface IsInForegroundCallback {
        /**
         * @param isInForeground 参考AutoMapConstant.AppRunStatus
         */
        void onAppInForeground(int isInForeground);
    }

    public void setCurrentImmersiveStatus(final int status) {
        mCurrentImmersiveStatus = status;
    }

    /**
     * 设置TMC数据
     *
     * @param naviTmcInfo NaviTmcInfo
     */
    public void setTmcData(final NaviTmcInfo naviTmcInfo) {
        Logger.i(TAG, "setTmcData");
        if (mNaviAdapter != null) {
            mNaviAdapter.setTmcData(naviTmcInfo);
        }
    }

    /**
     * 0:触摸态 1:沉浸态 -1:未知
     *
     * @return 返回当前的沉浸式状态
     */
    public int getCurrentImmersiveStatus() {
        return mCurrentImmersiveStatus;
    }

    /**
     * 三个参数，地点/路段a、起点b、终点c，a不空其他为空就是查询地点或道路的路况，a为空bc不为空就是查询b到c的路况，
     * ab为空c不空就是查询当前到c的路况，abc都为空就是查询前方路况
     *
     * @param a         String
     * @param b         String
     * @param c         String
     * @param mapTypeId 屏幕id
     * @return int 交通状态回调 -1：无数据 0:未知状态 1:通畅 2:缓慢 3:拥堵 4:严重拥堵 5:极度通畅
     */
    public int getTmcStatus(final String a, final String b, final String c,
                            final MapType mapTypeId) {
        Logger.i(TAG, "getTmcStatus a:" + a + " b:" + b + " c:" + c + " mapTypeId:" + mapTypeId);
        if (mNaviAdapter == null) {
            return -1;
        }
        return mNaviAdapter.getTmcStatus(a, b, c, mapTypeId);
    }

    public String getFrontTmcStatus() {
        if (mNaviAdapter == null) {
            return "";
        }
        return mNaviAdapter.getFrontTmcStatus();
    }

    public interface OnPreViewStatusChangeListener {
        /**
         * @param isPreView true:全览 false:非全览
         */
        void onPreViewStatusChange(boolean isPreView);
    }

    /**
     * @param listener OnPreViewStatusChangeListener
     */
    public void addOnPreviewStatusChangeListener(final OnPreViewStatusChangeListener listener) {
        if (listener != null && !mOnPreViewStatusChangeListeners.contains(listener)) {
            mOnPreViewStatusChangeListeners.add(listener);
        }
    }

    /**
     * @param listener OnPreViewStatusChangeListener
     */
    public void removeOnPreviewStatusChangeListener(
            final OnPreViewStatusChangeListener listener) {
        mOnPreViewStatusChangeListeners.remove(listener);
    }

    /**
     * @param status true:全览 false:非全览
     */
    public void setPreviewStatus(final boolean status) {
        if (mIsPreview == status) {
            return;
        }
        Logger.i(TAG, "setPreviewStatus status:" + status);
        mIsPreview = status;
        if (!ConvertUtils.isEmpty(mOnPreViewStatusChangeListeners)) {
            for (OnPreViewStatusChangeListener listener : mOnPreViewStatusChangeListeners) {
                listener.onPreViewStatusChange(status);
            }
        }
    }

    /**
     * @return true:全览 false:非全览
     */
    public boolean getPreviewStatus() {
        return mIsPreview;
    }

    /**
     * 获取道路交换结果
     *
     * @param roadName     道路名称
     * @param exchangeType 交换类型 0:不走此道路 1:走此道路
     * @param mapTypeId    屏幕id
     * @return NaviExchangeEntity
     */
    public NaviExchangeEntity getExchangeResult(final String roadName, final int exchangeType,
                                                final MapType mapTypeId) {
        final ExchangeHelper exchangeHelper = new ExchangeHelper();
        final NaviExchangeEntity naviExchangeEntity = exchangeHelper.getExchangeResult(
                roadName, exchangeType, mapTypeId);
        Logger.i(TAG, "getExchangeResult naviExchangeEntity:" +
                naviExchangeEntity.toString());
        return naviExchangeEntity;
    }

    /**
     * @param mapType 屏幕ID
     * @return 当前线路上的所有道路名称
     */
    public RoadName getAllRoadName(MapType mapType) {
        Logger.i(TAG, "getAllRoadName mapType = " + mapType);
        PathInfo pathInfo = OpenApiHelper.getCurrentPathInfo(mapType);
        if (pathInfo == null) {
            return null;
        }
        RoadNameAndTmcHelper roadNameAndTmcHelper = new RoadNameAndTmcHelper();
        RoadName roadName = roadNameAndTmcHelper.getAllRoadName(
                mapType,
                mRoadName,
                pathInfo.getPathID());
        mRoadName = roadName;
        return roadName;
    }

    /**
     * @param mapType       屏幕ID
     * @param pathId        道路唯一标识符
     * @param roadLinkIndex 道路连接索引
     * @return TMC状态值
     */
    public int getTmcByRoadLinkIndex(MapType mapType, long pathId, int roadLinkIndex) {
        PathInfo pathInfo = OpenApiHelper.getPathInfo(mapType, pathId);
        if (null == pathInfo) {
            return NumberUtils.NUM_ERROR;
        }
        if (null != mNaviAdapter) {
            // 获取Tmc数据
            NaviTmcInfo naviTmcInfo = mNaviAdapter.getTmcData();
            if (!ConvertUtils.isEmpty(naviTmcInfo)) {
                // 获取当前线路上的Tmc数据
                NaviTmcInfo.NaviLightBarInfo currentLightBarInfo = mNaviAdapter.
                        getCurrentLightBarInfo(naviTmcInfo.getLightBarInfo(), pathInfo);
                RoadNameAndTmcHelper roadNameAndTmcHelper = new RoadNameAndTmcHelper();
                return roadNameAndTmcHelper.getTmcByRoadLinkIndex(currentLightBarInfo,
                        roadLinkIndex);
            }
        }
        return NumberUtils.NUM_ERROR;
    }

    /**
     * 更新电量信息
     */
    public void updateBatteryInfo() {
        if (mNaviAdapter != null) {
            mNaviAdapter.updateBatteryInfo();
        }
    }

    /***
     * 此接口属于动态获取
     * @return 获取自动添加的途径点信息
     */
    public List<NaviViaEntity> getAllViaPoints() {
        if (mNaviAdapter == null) {
            return null;
        }
        PathInfo pathInfo = OpenApiHelper.getCurrentPathInfo(MapType.MAIN_SCREEN_MAIN_MAP);
        return mNaviAdapter.getAllViaPoints(pathInfo);
    }

    /**
     * @return 返回剩余红绿灯的数量 -1为异常状态
     */
    public int getRouteRemainLightCount() {
        if (mCurrentNaviEtaInfo != null) {
            return mCurrentNaviEtaInfo.getRouteRemainLightCount();
        }
        return NumberUtils.NUM_ERROR;
    }

    /**
     * @return 返回当前城市的城市代码
     */
    public int getCityCode() {
        if (null != mCurrentNaviEtaInfo) {
            return mCurrentNaviEtaInfo.cityCode;
        }
        return NumberUtils.NUM_ERROR;
    }

    /**
     * @return 返回Eta信息
     */
    public NaviEtaInfo getCurrentNaviEtaInfo() {
        return mCurrentNaviEtaInfo;
    }

    /**
     * 当前导航类型 -1:未知 0:GPS导航 1:模拟导航
     *
     * @return 返回当前的导航类型
     */
    public int getCurrentNaviType() {
        return mCurrentNaviType;
    }

    public void setCruiseVoiceIsOpen(final boolean isOpen) {
        Logger.i(TAG, "setCruiseVoiceIsOpen:" + isOpen);
        this.mCruiseVoiceIsOpen = isOpen;
    }

    /**
     * 高亮显示当前主路线的路径
     */
    public void showCurrentPath() {
        Logger.i(TAG, "showCurrentPath");
        long currentPathId = OpenApiHelper.getCurrentPathId(MapType.MAIN_SCREEN_MAIN_MAP);
        ArrayList<PathInfo> pathInfoList = new ArrayList<>(OpenApiHelper.getCurrentPathInfos());
        int selectIndex = NumberUtils.NUM_ERROR;
        for (int i = 0; i < pathInfoList.size(); i++) {
            if (pathInfoList.get(i).getPathID() == currentPathId) {
                selectIndex = i;
                break;
            }
        }
        if (selectIndex != NumberUtils.NUM_ERROR) {
            Logger.i(TAG, "showCurrentPath selectIndex = " + selectIndex);
            mNaviAdapter.updatePathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathInfoList, selectIndex);
        } else {
            Logger.i(TAG, "showCurrentPath currentPathId not found");
        }
    }

    public void showSelectPatch(final long newPathId) {
        Logger.i(TAG, "showSelectPatch");
        PathInfo selectPathInfo = OpenApiHelper.getPathInfo(
                MapType.MAIN_SCREEN_MAIN_MAP, newPathId);
        ArrayList<PathInfo> pathInfos = new ArrayList<>();
        pathInfos.add(selectPathInfo);
        if (!ConvertUtils.isEmpty(pathInfos) && null != selectPathInfo) {
            mNaviAdapter.updatePathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathInfos,
                    0);
        }
    }

    /**
     * 显示主路线加上推荐路线
     *
     * @param newPathId 新的路径id
     */
    public void showMainAndSuggestPath(final long newPathId) {
        Logger.i(TAG, "showMainAndSuggestPath");
        PathInfo currentPathInfo = OpenApiHelper.getCurrentPathInfo(MapType.MAIN_SCREEN_MAIN_MAP);
        PathInfo suggestPathInfo = OpenApiHelper.getPathInfo(
                MapType.MAIN_SCREEN_MAIN_MAP, newPathId);
        ArrayList<PathInfo> pathInfos = new ArrayList<>();
        pathInfos.add(currentPathInfo);
        pathInfos.add(suggestPathInfo);
        if (!ConvertUtils.isEmpty(pathInfos) && null != suggestPathInfo) {
            mNaviAdapter.updatePathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathInfos, 0);
        }
    }

    /**
     * 删除途经点
     *
     * @param mapTypeId 屏幕ID
     * @param pid       poiId
     */
    public void removeViaPoint(MapType mapTypeId, String pid) {
        Logger.i(TAG, "removeViaPoint pid = " + pid);
        mLayerAdapter.removeViaPoint(mapTypeId, pid);
    }

    public void pauseNavi() {
        if (mNaviAdapter == null) {
            return;
        }
        mNaviAdapter.pauseNavi();
    }

    public void resumeNavi() {
        if (mNaviAdapter == null) {
            return;
        }
        mNaviAdapter.resumeNavi();
    }

    public void setSimulationSpeed(int simulationSpeed) {
        if (mNaviAdapter == null) {
            return;
        }
        mNaviAdapter.setSimulationSpeed(simulationSpeed);
    }

    /**
     * @param mapTypeId 关闭比例尺
     */
    public void closeDynamicLevel(MapType mapTypeId) {
        if (null != mLayerAdapter) {
            mLayerAdapter.closeDynamicLevel(mapTypeId);
        }
    }

    /**
     * 停止语音播放
     */
    public void stopSpeech() {
        if (null != mSpeechAdapter) {
            mSpeechAdapter.stop();
        }
    }

    public void onNaviClose(boolean isNaviClose) {
        Logger.i(TAG, "onNaviClose isNaviClose = " + isNaviClose);
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onCloseNavi(isNaviClose);
                    }
                }
            }
        });
    }

    /**
     * 外部接口点击引导界面沿途搜按钮.
     *
     * @param mapType MapType，默认只有主图MAIN.
     */
    public void exportOpenPassBy(final MapType mapType) {
        Logger.i(TAG, "openPassByPage");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onPassByClick(mapType);
                    }
                }
            }
        });
    }

    /**
     * 播放补能提示语音
     *
     * @param tts  语音内容
     * @param type 语音类型
     */
    public void playChargeTips(String tts, int type) {
        Logger.i(TAG, "playChargeTips tts:" + tts + " type:" + type);
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onPlayChargeTips(tts, type);
                    }
                }
            }
        });
    }

    /**
     * 补能提示语音回调
     *
     * @param type 语音类型
     */
    public void chargeTipsCallBack(int type) {
        Logger.i(TAG, "chargeTipsCallBack type = " + type);
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onChargeTipsCallBack(type);
                    }
                }
            }
        });
    }

    public void closeNavi() {
        Logger.i(TAG, "closeNavi");
        String currentNaviStatus = mNavistatusAdapter.getCurrentNaviStatus();
        int muteStatus = SettingAdapter.getInstance().getConfigKeyMute();
        if (muteStatus == NumberUtils.NUM_1) {
            SettingAdapter.getInstance().setConfigKeyMute(NumberUtils.NUM_0);
        }
        if (currentNaviStatus.equals(NaviStatus.NaviStatusType.NAVING)) {
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        }
        setPreviewStatus(false);
        setFixedOverViewStatus(false);
        mLayerAdapter.clearAllSearchLayerItems(MapType.MAIN_SCREEN_MAIN_MAP);
        mLayerAdapter.setVisibleGuideSignalLight(MapType.MAIN_SCREEN_MAIN_MAP, false);
        clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
        mLayerAdapter.setStartPointVisible(MapType.MAIN_SCREEN_MAIN_MAP, true);
        mLayerAdapter.clearEndAreaPoint(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 清除路线
     *
     * @param mapTypeId 屏幕ID
     */
    public void clearRouteLine(final MapType mapTypeId) {
        ThreadManager.getInstance().execute(() -> {
            mLayerAdapter.clearRouteLine(mapTypeId);
            mLayerAdapter.setCarLogoVisible(mapTypeId, true);
        });
    }


    /**
     * 隐藏分歧备选路线
     *
     * @param pathId    路线唯一标识符
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public void setPathVisible(MapType mapType, long pathId, boolean isVisible) {
        mLayerAdapter.setPathVisible(mapType, pathId, isVisible);
    }

    /* 设置能量耗尽点扎标是否显示 只在全览态展示 */
    public void setRouteEnergyEmptyPointVisible(MapType mapTypeId, boolean isShow) {
        mLayerAdapter.setRouteEnergyEmptyPointVisible(mapTypeId, isShow);
    }

    public LayerItemRouteEndPoint getEndPoint() {
        return mEndPoint;
    }

    public void setEndPoint(LayerItemRouteEndPoint mEndPoint) {
        this.mEndPoint = mEndPoint;
    }
}
