package com.fy.navi.service.logicpaket.navi;


import android.graphics.Rect;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.route.RouteAdapter;
import com.fy.navi.service.adapter.speech.SpeechAdapter;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviExchangeEntity;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviStartType;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
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
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author fy
 * @version $Revision.*$
 */
public final class NaviPackage implements GuidanceObserver {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private NaviAdapter mNaviAdapter;
    private LayerAdapter mLayerAdapter;
    private SpeechAdapter mSpeechAdapter;
    private Hashtable<String, IGuidanceObserver> mGuidanceObservers;
    private NavistatusAdapter mNavistatusAdapter;
    private final HistoryManager mManager;
    private RouteAdapter mRouteAdapter;
    private boolean mIsMute = false;
    private boolean mIsNopOpen = false;
    private int mCurrentImmersiveStatus = -1;
    private boolean mIsPreview = false;
    private NaviEtaInfo mCurrentNaviEtaInfo;
    private boolean mIsFixedOverView;

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
        mSpeechAdapter = SpeechAdapter.getInstance();
        mNaviAdapter = NaviAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mRouteAdapter = RouteAdapter.getInstance();
        mManager = HistoryManager.getInstance();
        mManager.init();
        addIsInForegroundCallback();
    }

    /**
     * init引导服务
     */
    public void initNaviService() {
        mNaviAdapter.initNaviService();
        mNaviAdapter.registerObserver("NaviPackage", this);
    }

    /**
     * unInit引导服务
     */
    public void unInitNaviService() {
        mNaviAdapter.unInitNaviService();
    }

    /**
     * 开始导航
     * @param isSimulate 是否是模拟导航
     * @return 是否导航成功
     */
    public boolean startNavigation(final boolean isSimulate) {
        final boolean result = mNaviAdapter.startNavigation(isSimulate ?
                NaviStartType.NAVI_TYPE_SIMULATION : NaviStartType.NAVI_TYPE_GPS);
        if (result) {
            if (isSimulate) {
                mCurrentNaviType = NumberUtils.NUM_1;
            } else {
                mCurrentNaviType = NumberUtils.NUM_0;
            }
            mIsMute = false;
            mLayerAdapter.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, true);
            mLayerAdapter.setFollowMode(MapType.LAUNCHER_WIDGET_MAP, true);
            mLayerAdapter.setFollowMode(MapType.LAUNCHER_DESK_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapType.MAIN_SCREEN_MAIN_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapType.LAUNCHER_WIDGET_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapType.LAUNCHER_DESK_MAP, true);
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NAVING);
        } else {
            mCurrentNaviType = NumberUtils.NUM_ERROR;
        }
        return result;
    }

    /**
     * @return 是否停止导航成功
     */
    public boolean stopNavigation() {
        final boolean result = mNaviAdapter.stopNavigation();
        if (result) {
            mSpeechAdapter.stop();
            mLayerAdapter.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
            mLayerAdapter.setFollowMode(MapType.LAUNCHER_WIDGET_MAP, false);
            mLayerAdapter.setFollowMode(MapType.LAUNCHER_DESK_MAP, false);
        } else {
            Logger.w(TAG, "stopNavigation failed!");
        }
        return result;
    }

    /**
     * 注册GuidanceObserver回调，HMI层触发
     * @param key              key
     * @param guidanceObserver GuidanceObserver
     */
    public void registerObserver(final String key, final IGuidanceObserver guidanceObserver) {
        mGuidanceObservers.put(key, guidanceObserver);
    }

    /**
     * 移除GuidanceObserver回调，HMI层触发
     * @param key key
     */
    public void unregisterObserver(final String key) {
        mGuidanceObservers.remove(key);
    }

    /***
     * 添加导航记录
     * @param isCompleted 是否完成导航
     **/
    public void addNaviRecord(final boolean isCompleted) {
        final List<RouteParam> allPoiParamList = mRouteAdapter.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        final boolean isEmpty = ConvertUtils.isEmpty(allPoiParamList);
        Logger.i(TAG, "isCompleted= " + isCompleted + ",isEmpty：" + isEmpty);
        if (!isEmpty) {
            final RouteParam startParam = allPoiParamList.get(0);
            final RouteParam endParam = allPoiParamList.get(allPoiParamList.size() - 1);
            final History history = new History();
            history.setMKeyWord(endParam.getName());
            history.setMPoiId(endParam.getPoiID());
            history.setMStartPoiName(startParam.getName());
            history.setMEndPoiName(endParam.getName());
            history.setMStartPoint(startParam.getRealPos().toString());
            history.setMEndPoint(endParam.getRealPos().toString());
            history.setMType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY);
            history.setMIsCompleted(isCompleted);
            if (isCompleted) {
                mManager.insertOrReplace(history);
            } else {
                mManager.insertValue(history);
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
            mNaviAdapter.selectMainPathID(pathID);
        });
    }

    /*设置静音*/
    public void setMute(final boolean isMute) {
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
            final long l = mNaviAdapter.obtainSAPAInfo(isFindRemainPath);
            Logger.i(TAG, "obtainSAPAInfo: " + l);
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
        Logger.i(TAG, "onCrossImageInfo: ");
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
        Logger.i(TAG, "onPlayTTS: ");
        ThreadManager.getInstance().postUi(() -> {
            if (NaviAdapter.getInstance().getMCountDownLatch() != null) {
                NaviAdapter.getInstance().setSoundInfoEntity(info);
                NaviAdapter.getInstance().getMCountDownLatch().countDown();
            }
            if (mIsMute) return; // 静音开关
            //如果NOP打开并且是不播报的类型则不播报 TODO nop打开状态待接入
            if (mIsNopOpen && !TTSPlayHelper.allowToPlayWithNopOpen(info.getSoundType())) return;
            mSpeechAdapter.synthesize(info.getText());
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
        Logger.i(TAG, "onSelectMainPathStatus");
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
    public void onUpdateElecVehicleETAInfo(final List<FyElecVehicleETAInfo> infos) {
        Logger.i(TAG, "onUpdateElecVehicleETAInfo:" + (infos != null ? infos.size() : 0));
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (IGuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateElectVehicleETAInfo(infos);
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
                        guidanceObserver.onDriveReport(report);
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

    /**
     * @param surfaceViewId 屏幕id.
     * @param rect 路口矩形区域.
     */
    public void setRoadCrossRect(final MapType surfaceViewId, final Rect rect) {
        Logger.i(TAG, "setRoadCrossRect");
        mNaviAdapter.setRoadCrossRect(surfaceViewId, rect);
    }

    /**
     * 语音打开/关闭引导中路线全览.
     *
     * @param mapTypeId 屏幕id.
     * @param open  true-开启全览  false-关闭全览.
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
     * @param mapTypeId MapTypeId，对应底图.
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
            public void isAppInForeground(final boolean isInForeground) {
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
         * @param isInForeground true表示在前台，false表示在后台
         */
        void onAppInForeground(boolean isInForeground);
    }

    /**
     * true表示在前台，false表示在后台
     *
     * @return 是否在前台
     */
    public boolean getIsAppInForeground() {
        return BaseApplication.isAppInForeground();
    }

    public void setCurrentImmersiveStatus(final int status) {
        mCurrentImmersiveStatus = status;
    }

    /**
     * 设置TMC数据
     * @param naviTmcInfo NaviTmcInfo
     */
    public void setTmcData(final NaviTmcInfo naviTmcInfo) {
        Logger.i(TAG, "setTmcData");
        mNaviAdapter.setTmcData(naviTmcInfo);
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
     * @param a String
     * @param b String
     * @param c String
     * @param mapTypeId 屏幕id
     * @return int 交通状态回调 -1：无数据 0:未知状态 1:通畅 2:缓慢 3:拥堵 4:严重拥堵 5:极度通畅
     */
    public int getTmcStatus(final String a, final String b, final String c,
                            final MapType mapTypeId) {
        Logger.i(TAG, "getTmcStatus a:" + a + " b:" + b + " c:" + c + " mapTypeId:" +
                mapTypeId);
        return mNaviAdapter.getTmcStatus(a, b, c, mapTypeId);
    }

    public String getFrontTmcStatus() {
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
     * @param roadName 道路名称
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
     * 更新电量信息
     */
    public void updateBatteryInfo() {
        mNaviAdapter.updateBatteryInfo();
    }

    /***
     * 此接口属于动态获取
     * @return 获取自动添加的途径点信息
     */
    public List<NaviViaEntity> getAllViaPoints() {
        return mNaviAdapter.getAllViaPoints();
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
     * 当前导航类型 -1:未知 0:GPS导航 1:模拟导航
     * @return 返回当前的导航类型
     */
    public int getCurrentNaviType() {
        return mCurrentNaviType;
    }
}
