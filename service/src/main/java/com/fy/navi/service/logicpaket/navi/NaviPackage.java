package com.fy.navi.service.logicpaket.navi;


import android.graphics.Rect;

import com.android.utils.ConvertUtils;
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
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviStartType;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.ui.BaseApplication;
import com.fy.navi.ui.IsAppInForegroundCallback;

import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
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
    private int mCurrentImmersiveStatus = -1;
    private boolean mIsPreview = false;
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
     * @param isSimulate 是否是模拟导航
     * @return 是否导航成功
     */
    /*开始导航*/
    public boolean startNavigation(boolean isSimulate) {
        boolean result = mNaviAdapter.startNavigation(isSimulate ? NaviStartType.NAVI_TYPE_SIMULATION : NaviStartType.NAVI_TYPE_GPS);
        if (result) {
            mIsMute = false;
            mLayerAdapter.setFollowMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, true);
            mLayerAdapter.setFollowMode(MapTypeId.LAUNCHER_WIDGET_MAP, true);
            mLayerAdapter.setFollowMode(MapTypeId.LAUNCHER_DESK_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapTypeId.MAIN_SCREEN_MAIN_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapTypeId.LAUNCHER_WIDGET_MAP, true);
            mLayerAdapter.setVisibleGuideSignalLight(MapTypeId.LAUNCHER_DESK_MAP, true);
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NAVING);
        }
        return result;
    }

    /**
     * @return 是否停止导航成功
     */
    public boolean stopNavigation() {
        boolean result = mNaviAdapter.stopNavigation();
        if (result) {
            mSpeechAdapter.stop();
            mLayerAdapter.setFollowMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, false);
            mLayerAdapter.setFollowMode(MapTypeId.LAUNCHER_WIDGET_MAP, false);
            mLayerAdapter.setFollowMode(MapTypeId.LAUNCHER_DESK_MAP, false);
        } else {
            Logger.w(TAG, "stopNavigation failed!");
        }
        return result;
    }

    /**
     * 注册GuidanceObserver回调，HMI层触发
     */
    public void registerObserver(String key, IGuidanceObserver guidanceObserver) {
        mGuidanceObservers.put(key, guidanceObserver);
    }

    /*移除GuidanceObserver回调，HMI层触发*/
    public void unregisterObserver(String key) {
        mGuidanceObservers.remove(key);
    }

    /***添加导航记录***/
    public void addNaviRecord(boolean isCompleted) {
        List<RouteParam> allPoiParamList = mRouteAdapter.getAllPoiParamList(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        boolean isEmpty = ConvertUtils.isEmpty(allPoiParamList);
        Logger.i(TAG, "isCompleted= " + isCompleted + ",isEmpty：" + isEmpty);
        if (!isEmpty) {
            RouteParam startParam = allPoiParamList.get(0);
            RouteParam endParam = allPoiParamList.get(allPoiParamList.size() - 1);
            History history = new History();
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
        return Helper.ep;
    }

    private static final class Helper {
        private static final NaviPackage ep = new NaviPackage();
    }

    /**
     * @param pathID 换为主路线的id @thread main
     */
    public void selectMainPathID(long pathID) {
        ThreadManager.getInstance().postUi(() -> {
            mNaviAdapter.selectMainPathID(pathID);
        });
    }

    /*设置静音*/
    public void setMute(boolean isMute) {
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
    public void obtainSAPAInfo(boolean isFindRemainPath) {
        ThreadManager.getInstance().postUi(() -> {
            long l = mNaviAdapter.obtainSAPAInfo(isFindRemainPath);
        });
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        Logger.i(TAG, "onNaviInfo: ");
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
    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
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
    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
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
    public void onNaviArrive(long traceId, int naviType) {
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
    public void onPlayTTS(SoundInfoEntity info) {
        Logger.i(TAG, "onPlayTTS: ");
        ThreadManager.getInstance().postUi(() -> {
            if (NaviAdapter.getInstance().getMCountDownLatch() != null) {
                NaviAdapter.getInstance().setSoundInfoEntity(info);
                NaviAdapter.getInstance().getMCountDownLatch().countDown();
            }
            if (!mIsMute) {
                mSpeechAdapter.synthesize(info.getText());
            }
        });
    }

    @Override
    public void onManeuverInfo(NaviManeuverInfo respData) {
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
    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
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
    public void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity) {
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
    public void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
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
    public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
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
    public void onUpdateViaPass(long viaIndex) {
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
    public void onSelectMainPathStatus(long pathID, int result) {
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
    public void updateBroadcastParam(int broadcastType, boolean isDay) {
        Logger.i(TAG, "updateBroadcastParam");
        mNaviAdapter.updateBroadcastParam(broadcastType, isDay);
    }

    @Override
    public void onCurrentRoadSpeed(int speed) {
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

    // 透出透出电动车ETA信息
    @Override
    public void onUpdateElecVehicleETAInfo(List<FyElecVehicleETAInfo> infos) {
        Logger.i(TAG, "onUpdateElecVehicleETAInfo:" + (infos != null ? infos.size() : 0));
        // TODO 待UI设计
    }

    @Override
    public void onDriveReport(NaviDriveReportEntity report) {
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

    public void setRoadCrossRect(MapTypeId surfaceViewId, Rect rect) {
        Logger.i(TAG, "setRoadCrossRect");
        mNaviAdapter.setRoadCrossRect(surfaceViewId, rect);
    }

    /**
     * 语音打开/关闭引导中路线全览.
     *
     * @param mapTypeId，对应底图.
     * @param open  true-开启全览  false-关闭全览.
     */
    public void voiceRouteOverview(MapTypeId mapTypeId, boolean open) {
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
     * @param parallelOption，切换类型，MAIN-主路 SIDE-辅路  ON-桥上  UNDER-桥下.
     */
    public void voiceChangeParallelRoad(MapTypeId mapTypeId, String parallelOption) {
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
    public void voiceContinueNavigation(MapTypeId mapTypeId) {
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

    private void addIsInForegroundCallback() {
        Logger.i(TAG, "addIsInForegroundCallback");
        BaseApplication.addIsAppInForegroundCallback(new IsAppInForegroundCallback() {
            @Override
            public void isAppInForeground(boolean isInForeground) {
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
    public void addIsAppInForegroundCallback(IsInForegroundCallback callback) {
        Logger.i(TAG, "addIsAppInForegroundCallback callback = " + callback);
        if (callback != null) {
            if (!mIsAppInForegroundCallbacks.contains(callback)) {
                mIsAppInForegroundCallbacks.add(callback);
            }
        }
    }

    public void removeIsAppInForegroundCallback(IsInForegroundCallback callback) {
        Logger.i(TAG, "removeIsAppInForegroundCallback callback = " + callback);
        if (callback != null) {
            mIsAppInForegroundCallbacks.remove(callback);
        }
    }

    public interface IsInForegroundCallback {
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

    public void setCurrentImmersiveStatus(int status) {
        mCurrentImmersiveStatus = status;
    }

    /**
     * 设置TMC数据
     * @param naviTmcInfo NaviTmcInfo
     */
    public void setTmcData(NaviTmcInfo naviTmcInfo) {
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
     * @return int 交通状态回调 -1：无数据 0:未知状态 1:通畅 2:缓慢 3:拥堵 4:严重拥堵 5:极度通畅
     */
    public int getTmcStatus(String a, String b, String c, MapTypeId mapTypeId) {
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
    public void addOnPreviewStatusChangeListener(OnPreViewStatusChangeListener listener) {
        if (listener != null && !mOnPreViewStatusChangeListeners.contains(listener)) {
            mOnPreViewStatusChangeListeners.add(listener);
        }
    }

    /**
     * @param listener OnPreViewStatusChangeListener
     */
    public void removeOnPreviewStatusChangeListener(OnPreViewStatusChangeListener listener) {
        mOnPreViewStatusChangeListeners.remove(listener);
    }

    /**
     * @param status true:全览 false:非全览
     */
    public void setPreviewStatus(boolean status) {
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

    public boolean getPreviewStatus() {
        return mIsPreview;
    }
}
