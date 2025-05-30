package com.fy.navi.service.logicpaket.navi;


import android.graphics.Rect;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.route.RouteAdapter;
import com.fy.navi.service.adapter.setting.SettingAdapter;
import com.fy.navi.service.adapter.signal.SignalAdapter;
import com.fy.navi.service.adapter.signal.SignalAdapterCallback;
import com.fy.navi.service.adapter.speech.SpeechAdapter;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviExchangeEntity;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviRoadFacilityEntity;
import com.fy.navi.service.define.navi.NaviStartType;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.RoadName;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navi.SuggestChangePathReasonEntity;
import com.fy.navi.service.define.navi.TrafficLightCountdownEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.usertrack.HistoryPoiItemBean;
import com.fy.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import com.fy.navi.service.tts.TTSPlayHelper;
import com.fy.navi.ui.BaseApplication;
import com.fy.navi.ui.IsAppInForegroundCallback;

import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;

import lombok.Getter;

/**
 * @author fy
 * @version $Revision.*$
 */
public final class NaviPackage implements GuidanceObserver, SignalAdapterCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private NaviAdapter mNaviAdapter;
    private LayerAdapter mLayerAdapter;
    private SpeechAdapter mSpeechAdapter;
    private SettingAdapter mSettingAdapter;
    private Hashtable<String, IGuidanceObserver> mGuidanceObservers;
    private NavistatusAdapter mNavistatusAdapter;
    private final HistoryManager mManager;
    private RouteAdapter mRouteAdapter;
    private UserTrackAdapter mUserTrackAdapter;
    private SignalAdapter mSignalAdapter;
    private boolean mIsMute = false;
    private boolean mIsNopOpen = false;
    private int mCurrentImmersiveStatus = -1;
    private boolean mIsPreview = false;
    @Getter
    private NaviEtaInfo mCurrentNaviEtaInfo;
    private boolean mIsFixedOverView;
    private boolean mCruiseVoiceIsOpen = true; // 巡航播报是否开启
    // 线路上所有道路的名称
    private RoadName mRoadName;
    /**
     * 当前导航类型 -1:未知 0:GPS导航 1:模拟导航
     */
    private int mCurrentNaviType = NumberUtils.NUM_ERROR;
    private List<IsInForegroundCallback> mIsAppInForegroundCallbacks =
            new CopyOnWriteArrayList<>();
    private List<OnPreViewStatusChangeListener> mOnPreViewStatusChangeListeners =
            new CopyOnWriteArrayList<>();

    private NaviPackage() {
        mGuidanceObservers = new Hashtable<>();
        mManager = HistoryManager.getInstance();
        mManager.init();
        addIsInForegroundCallback();
    }

    /**
     * init引导服务
     */
    public void initNaviService() {
        mSpeechAdapter = SpeechAdapter.getInstance();
        mNaviAdapter = NaviAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mRouteAdapter = RouteAdapter.getInstance();
        mSettingAdapter = SettingAdapter.getInstance();
        mUserTrackAdapter = UserTrackAdapter.getInstance();
        mSignalAdapter = SignalAdapter.getInstance();
        mNaviAdapter.initNaviService();
        mNaviAdapter.registerObserver("NaviPackage", this);
        mSignalAdapter.registerCallback("NaviPackage", this);
    }

    /**
     * unInit引导服务
     */
    public void unInitNaviService() {
        if(mNaviAdapter != null) {
            mNaviAdapter.unInitNaviService();
        }
    }

    /**
     * 开始导航
     *
     * @param isSimulate 是否是模拟导航
     * @return 是否导航成功
     */
    public boolean startNavigation(final boolean isSimulate) {
        if(mNaviAdapter == null) {
            Logger.e(TAG, "startNavigation", "mNaviAdapter == null");
            return false;
        }
        final boolean result = mNaviAdapter.startNavigation(isSimulate ? NaviStartType.NAVI_TYPE_SIMULATION : NaviStartType.NAVI_TYPE_GPS);
        final String currentNaviStatus = mNavistatusAdapter.getCurrentNaviStatus();
        Logger.i(TAG, "startNavigation", "result:" + result, "isSimulate:" + isSimulate, "currentNaviStatus:" + currentNaviStatus);
        if (result) {
            if (isSimulate) {
                mCurrentNaviType = NumberUtils.NUM_1;
                mNaviAdapter.setSimulationSpeed(40);
            } else {
                mCurrentNaviType = NumberUtils.NUM_0;
            }
            mLayerAdapter.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, true);
            mLayerAdapter.setFollowMode(MapType.LAUNCHER_WIDGET_MAP, true);
            mLayerAdapter.setFollowMode(MapType.LAUNCHER_DESK_MAP, true);
            mLayerAdapter.setFollowMode(MapType.CLUSTER_MAP, true);
            mLayerAdapter.setFollowMode(MapType.HUD_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapType.MAIN_SCREEN_MAIN_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapType.LAUNCHER_WIDGET_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapType.LAUNCHER_DESK_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapType.CLUSTER_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapType.HUD_MAP, true);
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NAVING);
            PathInfo pathInfo = mRouteAdapter.getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP) ==
                    null ? null :
                    (PathInfo) Objects.requireNonNull(mRouteAdapter.
                            getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP)).getMPathInfo();
            OpenApiHelper.setCurrentPathInfo(pathInfo);
            ArrayList<PathInfo> list = new ArrayList<>();
            list.add(pathInfo);
            if (!ConvertUtils.isEmpty(list) && null != pathInfo) {
                //1046394 导航启动慢优化
                ThreadManager.getInstance().execute(new Runnable() {
                    @Override
                    public void run() {
                        updatePathInfo(MapType.MAIN_SCREEN_MAIN_MAP, list, 0);
                        updatePathInfo(MapType.LAUNCHER_WIDGET_MAP, list, 0);
                        updatePathInfo(MapType.LAUNCHER_DESK_MAP, list, 0);
                        updatePathInfo(MapType.CLUSTER_MAP, list, 0);
                        updatePathInfo(MapType.HUD_MAP, list, 0);
                    }
                });
            }
        } else {
            mCurrentNaviType = NumberUtils.NUM_ERROR;
        }
        mRouteAdapter.sendL2Data(MapType.MAIN_SCREEN_MAIN_MAP);
        return result;
    }

    /**
     * @return 是否停止导航成功
     */
    public boolean stopNavigation() {
        Logger.i(TAG, "stopNavigation");
        if(mNaviAdapter == null) {
            Logger.e(TAG, "stopNavigation", "mNaviAdapter == null");
            return false;
        }
        final boolean result = mNaviAdapter.stopNavigation();
        if (result) {
            mLayerAdapter.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
            mLayerAdapter.setFollowMode(MapType.LAUNCHER_WIDGET_MAP, false);
            mLayerAdapter.setFollowMode(MapType.LAUNCHER_DESK_MAP, false);
            mLayerAdapter.setFollowMode(MapType.CLUSTER_MAP, false);
            mLayerAdapter.setFollowMode(MapType.HUD_MAP, false);
            mCurrentNaviType = NumberUtils.NUM_ERROR;
            sendBuryPointForCloseNavi();
        } else {
            Logger.w(TAG, "stopNavigation failed!");
        }
        return result;
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_END_MANUAL)
    private void sendBuryPointForCloseNavi(){
        NaviEtaInfo mNaviEtaInfo = getCurrentNaviEtaInfo();
        if(mNaviEtaInfo == null){
            Logger.e(MapDefaultFinalTag.NAVI_BURY_POINT, "eventName-" + BuryConstant.EventName.AMAP_NAVI_END_MANUAL + ": mNaviEtaInfo is null");
            return;
        }
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_REMAINING_TIME, TimeUtils.getArriveTime(AppContext.getInstance().getMContext(), mNaviEtaInfo.getAllTime()))
                .setParams(BuryConstant.ProperType.BURY_KEY_TRIP_DISTANCE, TimeUtils.getRemainInfo(AppContext.getInstance().getMContext(), mNaviEtaInfo.getAllDist(), mNaviEtaInfo.getAllTime()))
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
                    mCurrentNaviEtaInfo.getAllDist() < 1000) {//添加未完成导航(车辆熄火前的CVP距离目的地的距离 ≥ 1 km)
                Logger.i(TAG, "isCompleted= ");
                return;
            }
            addNaviRecord(false);
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

    /***
     * 添加导航记录
     * @param isCompleted 是否完成导航 由于逻辑调整，现对此做出解释，默认插入时都为true，只有在接收下电信号时如果距离大于1KM则为false
     **/
    public void addNaviRecord(final boolean isCompleted) {
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
            if (isCompleted) {
                mManager.insertOrReplace(history);
            } else {
                mManager.insertValue(history);
            }
            // 如果登陆了话就将数据传给高德
            if (mUserTrackAdapter.isLogin()) {
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
                historyRouteItemBean.setToPoi(endPoiItemBean);
                mUserTrackAdapter.addHistoryRoute(historyRouteItemBean);
            }
        }
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

    private static final class Helper {
        private static final NaviPackage NAVI_PACKAGE = new NaviPackage();
    }

    /**
     * @param pathID 换为主路线的id @thread main
     */
    public void selectMainPathID(final long pathID) {
        ThreadManager.getInstance().postUi(() -> {
            if(mNaviAdapter != null) {
                mNaviAdapter.selectMainPathID(pathID);
            }
        });
    }

    /**
     * 地图上选择路线，切换path加上路线绘制
     *
     * @param mapTypeId 屏幕id
     * @param pathId    路线id
     * @param pathIndex 路线索引
     */
    public void selectPath(final MapType mapTypeId, final long pathId, final int pathIndex) {
        Logger.i(TAG, "selectPath: " + pathId + ",pathIndex:" + pathIndex + "mapId = " +
                mapTypeId);
        selectMainPathID(pathId);
        mLayerAdapter.setSelectedPathIndex(mapTypeId, pathIndex);
    }

    /*设置静音*/
    public void setMute(final boolean isMute) {
        mSettingAdapter.setConfigKeyMute(isMute? 1 : 0);
        mIsMute = isMute;
    }

    /*是否静音*/
    public boolean isMute() {
        return mIsMute;
    }

    /**
     * 获取服务区和收费站信息
     *
     * @param isFindRemainPath 是否查询剩余路线  @thread main
     */
    public void obtainSAPAInfo(final boolean isFindRemainPath) {
        ThreadManager.getInstance().postUi(() -> {
            if(mNaviAdapter != null) {
                final long l = mNaviAdapter.obtainSAPAInfo(isFindRemainPath);
                Logger.i(TAG, "obtainSAPAInfo: " + l);
            }
        });
    }

    @Override
    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        Logger.i(TAG, "onNaviInfo: ");
        mCurrentNaviEtaInfo = naviETAInfo;
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onNaviInfo(naviETAInfo);
                    }
                }
            }
        });
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
        Logger.i(TAG, "onLaneInfo: " + (isShowLane));
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onLaneInfo(isShowLane, laneInfoEntity);
                    }
                }
            }
        });
    }

    @Override
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        Logger.i(TAG, "onCrossImageInfo isShowImage:" + isShowImage + " naviImageInfo:" + naviImageInfo);
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
    public void onNaviArrive(final long traceId, final int naviType) {
        Logger.i(TAG, "onNaviArrive: traceId = " + traceId + ", naviType = " + naviType);
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
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onNaviStop();
                    }
                }
            }
        });
    }

    @Override
    public void onPlayTTS(final SoundInfoEntity info) {
        Logger.i(TAG, "onPlayTTS: " + (info == null ? "" : info.getText()));
        if(info == null){
            return;
        }
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
                if (mIsMute) return; // 静音开关
                //如果NOP打开并且是不播报的类型则不播报
                if (SignalPackage.getInstance().getNavigationOnAdasTextToSpeachStatus() == 1 && !TTSPlayHelper.allowToPlayWithNopOpen(info.getSoundType()))
                    return;
                mSpeechAdapter.synthesize(!info.isHighPriority(), info.getText());
            } catch (Exception e) {
                Logger.e(TAG, "playTTs exception:" + e.getMessage());
            }
        });
    }

    @Override
    public void onManeuverInfo(final NaviManeuverInfo respData) {
        Logger.i(TAG, "onManeuverInfo");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
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
        Logger.i(TAG, "onNaviSAPAInfo entity = " + sapaInfoEntity.toString());
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
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
        Logger.i(TAG, "updateBroadcastParam");
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
        Logger.i(TAG, "onUpdateElectVehicleETAInfo:" + (infos != null ? infos.size() : 0));
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateElectVehicleETAInfo(infos);
                }
            }
        }
    }

    @Override
    public void onUpdateTrafficLightCountdown(final ArrayList<TrafficLightCountdownEntity> list) {
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
    public void onDriveReport(final NaviDriveReportEntity report) {
        Logger.i(TAG, "onDriveReport = " + report.toString());
        Logger.i(TAG, "onNaviSAPAInfo");
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        // 因为没有行车报告页面需求，所以这边先注释了
//                        guidanceObserver.onDriveReport(report);
                    }
                }
            }
        });
    }

    @Override
    public void onDeletePath(ArrayList<Long> pathIDList) {
        if (ConvertUtils.isEmpty(pathIDList)) {
            return;
        }
        Logger.i(TAG, GsonUtils.toJson(pathIDList));
        for (Long pathId : pathIDList) {
            //todo清除该路线
        }
    }

    @Override
    public void onChangeNaviPath(long oldPathId, long pathID) {
        Logger.i(TAG, "onChangeNaviPath oldPathId = " + oldPathId + " pathID = " + pathID);
        PathInfo pathInfo = OpenApiHelper.getPathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathID);
        OpenApiHelper.setCurrentPathInfo(pathInfo);
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
    public void onSuggestChangePath(long newPathID, long oldPathID, SuggestChangePathReasonEntity reason) {
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

    /**
     * @param surfaceViewId 屏幕id.
     * @param rect          路口矩形区域.
     */
    public void setRoadCrossRect(final MapType surfaceViewId, final Rect rect) {
        Logger.i(TAG, "setRoadCrossRect");
        mLayerAdapter.updateRoadCrossRect(surfaceViewId, rect);
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

    /**
     * 添加应用前后台状态监听回调.
     */
    private void addIsInForegroundCallback() {
        Logger.i(TAG, "addIsInForegroundCallback");
        BaseApplication.addIsAppInForegroundCallback(new IsAppInForegroundCallback() {
            @Override
            public void isAppInForeground(final int isInForeground) {
                Logger.i(TAG, "isAppInForeground: " + isInForeground);
                if (!ConvertUtils.isEmpty(mIsAppInForegroundCallbacks)) {
                    for (IsInForegroundCallback isInForegroundCallback :
                            mIsAppInForegroundCallbacks) {
                        isInForegroundCallback.onAppInForeground(isInForeground);
                    }
                }
            }
        });
    }

    /**
     * 添加是否在前台的回调
     *
     * @param callback callback
     */
    public void addIsAppInForegroundCallback(final IsInForegroundCallback callback) {
        Logger.i(TAG, "addIsAppInForegroundCallback callback = " + callback);
        if (callback != null) {
            if (!mIsAppInForegroundCallbacks.contains(callback)) {
                mIsAppInForegroundCallbacks.add(callback);
            }
        }
    }

    /**
     * @param callback callback
     */
    public void removeIsAppInForegroundCallback(final IsInForegroundCallback callback) {
        Logger.i(TAG, "removeIsAppInForegroundCallback callback = " + callback);
        if (callback != null) {
            mIsAppInForegroundCallbacks.remove(callback);
        }
    }

    public interface IsInForegroundCallback {
        /**
         * @param isInForeground 参考AutoMapConstant.AppRunStatus
         */
        void onAppInForeground(int isInForeground);
    }

    /**
     *
     * @return 参考AutoMapConstant.AppRunStatus.
     */
    public int getIsAppInForeground() {
        return BaseApplication.isAppInForeground();
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
        if(mNaviAdapter != null) {
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
        if(mNaviAdapter == null) {
            return -1;
        }
        return mNaviAdapter.getTmcStatus(a, b, c, mapTypeId);
    }

    public String getFrontTmcStatus() {
        if(mNaviAdapter == null) {
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
    public void removeOnPreviewStatusChangeListener(final OnPreViewStatusChangeListener listener) {
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
     * @param mapType 屏幕ID
     * @param pathId 道路唯一标识符
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
        if(mNaviAdapter != null) {
            mNaviAdapter.updateBatteryInfo();
        }
    }

    /***
     * 此接口属于动态获取
     * @return 获取自动添加的途径点信息
     */
    public List<NaviViaEntity> getAllViaPoints() {
        if(mNaviAdapter == null) {
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
     * 更新引导路线数据
     *
     * @param pathInfoList 路线数据
     * @param selectIndex  选中下标
     */
    public boolean updatePathInfo(final MapType mapTypeId, final ArrayList<?> pathInfoList,
                                  final int selectIndex) {
        Logger.i(TAG, "updatePathInfo pathInfoList.size = " +
                (!ConvertUtils.isEmpty(pathInfoList) ? pathInfoList.size() : 0) +
                " selectIndex = " + selectIndex + " mapTypeId = " + mapTypeId);
        if (null != mLayerAdapter) {
            mLayerAdapter.updatePathInfo(mapTypeId, pathInfoList, selectIndex);
        }
        return false;
    }

    /**
     * 显示当前主路线的路径
     */
    public void onlyShowCurrentPath() {
        Logger.i(TAG, "onlyShowCurrentPath");
        PathInfo pathInfo = OpenApiHelper.getCurrentPathInfo(MapType.MAIN_SCREEN_MAIN_MAP);
        if (null == pathInfo) {
            Logger.i(TAG, "onlyShowCurrentPath pathInfo is null");
            return;
        }
        ArrayList<PathInfo> pathInfoList = new ArrayList<>();
        pathInfoList.add(pathInfo);
        updatePathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathInfoList, 0);
    }

    public void showSelectPatch(final long newPathId) {
        Logger.i(TAG, "showSelectPatch");
        PathInfo selectPathInfo = OpenApiHelper.getPathInfo(
                MapType.MAIN_SCREEN_MAIN_MAP, newPathId);
        ArrayList<PathInfo> pathInfos = new ArrayList<>();
        pathInfos.add(selectPathInfo);
        if (!ConvertUtils.isEmpty(pathInfos) && null != selectPathInfo) {
            updatePathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathInfos,
                    0);
        }
    }

    /**
     * 显示主路线加上推荐路线
     *
     * @param newPathId 新的路径id
     */
    public void showMainAndSuggestPath(final long newPathId) {
        // TODO 后续这边要加上推荐路线的推荐扎标，比如快多少分钟等等的
        Logger.i(TAG, "showMainAndSuggestPath");
        PathInfo currentPathInfo = OpenApiHelper.getCurrentPathInfo(MapType.MAIN_SCREEN_MAIN_MAP);
        PathInfo suggestPathInfo = OpenApiHelper.getPathInfo(
                MapType.MAIN_SCREEN_MAIN_MAP, newPathId);
        ArrayList<PathInfo> pathInfos = new ArrayList<>();
        pathInfos.add(currentPathInfo);
        pathInfos.add(suggestPathInfo);
        if (!ConvertUtils.isEmpty(pathInfos) && null != suggestPathInfo) {
            updatePathInfo(MapType.MAIN_SCREEN_MAIN_MAP, pathInfos, 0);
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
        if(mNaviAdapter == null) {
            return;
        }
        mNaviAdapter.pauseNavi();
    }

    public void resumeNavi() {
        if(mNaviAdapter == null) {
            return;
        }
        mNaviAdapter.resumeNavi();
    }

    public void setSimulationSpeed(int simulationSpeed) {
        if(mNaviAdapter == null) {
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
}
