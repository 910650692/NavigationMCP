package com.sgm.navi.service.adapter.navi.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.path.model.ElecVehicleETAInfo;
import com.autonavi.gbl.common.path.model.TollGateInfo;
import com.autonavi.gbl.common.path.option.RouteOption;
import com.autonavi.gbl.common.path.option.RouteType;
import com.autonavi.gbl.guide.model.CrossImageInfo;
import com.autonavi.gbl.guide.model.DriveReport;
import com.autonavi.gbl.guide.model.ExitDirectionInfo;
import com.autonavi.gbl.guide.model.LaneInfo;
import com.autonavi.gbl.guide.model.LightBarDetail;
import com.autonavi.gbl.guide.model.LightBarInfo;
import com.autonavi.gbl.guide.model.ManeuverIconResponseData;
import com.autonavi.gbl.guide.model.ManeuverInfo;
import com.autonavi.gbl.guide.model.MixForkInfo;
import com.autonavi.gbl.guide.model.NaviCameraExt;
import com.autonavi.gbl.guide.model.NaviCongestionInfo;
import com.autonavi.gbl.guide.model.NaviFacility;
import com.autonavi.gbl.guide.model.NaviGreenWaveCarSpeed;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.guide.model.NaviIntervalCameraDynamicInfo;
import com.autonavi.gbl.guide.model.NaviRoadFacility;
import com.autonavi.gbl.guide.model.NaviWeatherInfo;
import com.autonavi.gbl.guide.model.PathTrafficEventInfo;
import com.autonavi.gbl.guide.model.SAPAInquireResponseData;
import com.autonavi.gbl.guide.model.SoundInfo;
import com.autonavi.gbl.guide.model.SuggestChangePathReason;
import com.autonavi.gbl.guide.model.TrafficLightCountdown;
import com.autonavi.gbl.guide.model.WeatherInfo;
import com.autonavi.gbl.guide.observer.INaviObserver;
import com.autonavi.gbl.guide.observer.ISoundPlayObserver;
import com.autonavi.gbl.util.model.BinaryStream;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.R;
import com.sgm.navi.service.adapter.navi.GuidanceObserver;
import com.sgm.navi.service.adapter.navi.NaviAdapter;
import com.sgm.navi.service.define.navi.CameraInfoEntity;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.FyElecVehicleETAInfo;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviCongestionInfoEntity;
import com.sgm.navi.service.define.navi.NaviDriveReportEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviMixForkInfo;
import com.sgm.navi.service.define.navi.NaviRoadFacilityEntity;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.navi.SoundInfoEntity;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navi.SuggestChangePathReasonEntity;
import com.sgm.navi.service.define.navi.TrafficLightCountdownEntity;
import com.sgm.navi.service.define.route.RouteWeatherInfo;
import com.sgm.navi.service.tts.NaviAudioPlayer;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.ScheduledFuture;

/**
 * 导航信息观察者
 *
 * @author sgm
 * @version $Revision.*$
 */
public class GuidanceCallback implements INaviObserver, ISoundPlayObserver {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_CALLBACK;
    private Hashtable<String, GuidanceObserver> mGuidanceObservers;
    private NaviApiImplHelper mHelper;
    private ScheduledFuture mScheduledFuture;

    public GuidanceCallback(final Hashtable<String, GuidanceObserver> guidanceObservers, final NaviApiImplHelper helper) {
        this.mGuidanceObservers = guidanceObservers;
        this.mHelper = helper;
    }

    /***
     * 页面倒计时
     */
    public void onBatterHotTime() {
        cancelTimer();
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> onBatterHotCallBack(false), 0, 10);
    }

    /***
     * 取消页面倒计时
     */
    private void cancelTimer() {
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    public void startNavi() {
        if (ConvertUtils.isEmpty(mGuidanceObservers)) return;
        for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
            if (guidanceObserver != null) {
                guidanceObserver.onNaviStart();
            }
        }
    }

    /**
     * 一秒回调一次，返回所有可选线路的信息（条条大路通罗马）
     *
     * @param naviInfoList 导航信息列表
     */
    @Override
    public void onUpdateNaviInfo(final ArrayList<NaviInfo> naviInfoList) {
        mHelper.mNaviInfo = ConvertUtils.isEmpty(naviInfoList) ? null : naviInfoList.get(0);
        NaviAdapter.getInstance().setNaviInfoList(NaviDataFormatHelper.
                forMatNaviInfoEntity(naviInfoList));
        final NaviEtaInfo naviETAInfo = NaviDataFormatHelper.forMatNaviInfo(naviInfoList);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviInfo(naviETAInfo);
                }
            }
        }
    }

    @Override
    public void onShowNaviManeuver(final ManeuverInfo info) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            NaviManeuverInfo maneuverInfo = NaviDataFormatHelper.formatManeuverInfo(info);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onManeuverInfo(maneuverInfo);
                }
            }
        }
    }

    @Override
    public void onObtainManeuverIconData(final ManeuverIconResponseData respData) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            NaviManeuverInfo maneuverInfo = NaviDataFormatHelper.formatManeuverIconData(respData);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onManeuverInfo(maneuverInfo);
                }
            }
        }
    }

    @Override
    public void onUpdateExitDirectionInfo(final ExitDirectionInfo boardInfo) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            NaviManeuverInfo naviManeuverInfo = NaviDataFormatHelper.formatExitDirectionInfo(boardInfo);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onManeuverInfo(naviManeuverInfo);
                }
            }
        }
    }

    @Override
    public void onShowTollGateLane(final TollGateInfo tollGateInfo) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            SapaInfoEntity sapaInfoEntity =  NaviDataFormatHelper.formatTollGateInfo(tollGateInfo);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSAPAInfo(sapaInfoEntity);
                }
            }
        }
    }

    @Override
    public void onShowCrossImage(final CrossImageInfo info) {
        if(null == info) {
            Logger.i(TAG, "CrossImageInfo is null");
            return;
        }
        final CrossImageEntity naviImageInfo = NaviDataFormatHelper.forMatImageInfo(info);
        if(null == naviImageInfo){
            Logger.i(TAG, "naviImageInfo is null:");
            return;
        }
        long distance = naviImageInfo.getDistance();
        Logger.i(TAG, "onShowCrossImage naviImageInfo:", distance);
        boolean isHaveNaviImageInfo = distance > 0;
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onShowCrossImage(info);
                    guidanceObserver.onCrossImageInfo(isHaveNaviImageInfo, naviImageInfo);
                }
            }
        }
    }

    @Override
    public void onHideCrossImage(final int type) {
        Logger.i(TAG, "onHideCrossImage type:", type);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            CrossImageEntity crossImageEntity =  NaviDataFormatHelper.forMatImageInfo(type);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onCrossImageInfo(false, crossImageEntity);
                }
            }
        }
    }

    @Override
    public void onShowNaviCrossTMC(final BinaryStream dataBuf) {

    }

    @Override
    public void onPassLast3DSegment() {

    }

    @Override
    public void onShowNaviLaneInfo(final LaneInfo info) {
        final LaneInfoEntity laneInfoEntity = NaviDataFormatHelper.forMatLaneInfo(info);
        boolean isHaveLaneInfo = !ConvertUtils.isEmpty(laneInfoEntity);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onLaneInfo(isHaveLaneInfo, laneInfoEntity);
                }
            }
        }
    }

    @Override
    public void onHideNaviLaneInfo() {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onLaneInfo(false, null);
                }
            }
        }
    }

    @Override
    public void onUpdateSAPA(final ArrayList<NaviFacility> serviceAreaList) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            SapaInfoEntity sapaInfoEntity = NaviDataFormatHelper.forMatSAPAInfo(serviceAreaList);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSAPAInfo(sapaInfoEntity);
                }
            }
        }
    }

    @Override
    public void onObtainSAPAInfo(final SAPAInquireResponseData respData) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            SapaInfoEntity sapaInfoEntity = NaviDataFormatHelper.forMatSAPAInfo(respData);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSAPAInfo(sapaInfoEntity);
                }
            }
        }
    }

    @Override
    public void onNaviStop(final long traceId, final int naviType) {
        Logger.i(TAG, "GuidanceCallback onNaviStop: id={?}, naviType={?}", traceId, naviType);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviStop();
                }
            }
        }
    }

    @Override
    public void onUpdateViaPass(final long viaIndex) {
        Logger.i(TAG, "GuidanceCallback onUpdateViaPass: viaIndex={?}", viaIndex);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateViaPass(viaIndex);
                }
            }
        }
    }

    private void onBatterHotCallBack(boolean isPass) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onBatterHotCallBack(isPass);
                }
            }
        }
    }

    @Override
    public void onUpdateChargeStationPass(long viaIndex) {
        INaviObserver.super.onUpdateChargeStationPass(viaIndex);
        // 充电站索引为路线信息中充电站列表的数组下标，它仅包含接续算路下发的充电站，不一定包括用户添加的充电站途经点。
        // 当充电站是个途经点时，当作途经点处理，只回调 OnUpdateViaPass()，不会回调 OnUpdateChargeStationPass()。
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateChargeStationPass(viaIndex);
                }
            }
        }
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_END_AUTO)
    public void onNaviArrive(final long traceId, final int naviType) {
        Logger.i(TAG, "GuidanceCallback onNaviArrive: id={?}, naviType={?}", traceId, naviType);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviArrive(traceId, naviType);
                }
            }
        }
    }

    /**
     * 光柱图回调接口.
     *
     * @param lightBarInfo   表示光柱图的分段路况信息（只透出未行驶路段的路况信息（不包含已行驶路段））
     * @param lightBarDetail 提供光柱图的详细信息，例如每段路况的状态和长度
     * @param passedIdx      表示已行驶过的路段索引
     * @param dataStatus     数据状态 true：表示路况信息发生变化，需要刷新路线路况 false:表示路况信息无变化
     */
    @Override
    public void onUpdateTMCLightBar(final ArrayList<LightBarInfo> lightBarInfo,
                                    final LightBarDetail lightBarDetail, final long passedIdx,
                                    final boolean dataStatus) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            NaviTmcInfo naviTmcInfo = NaviDataFormatHelper.formatTmcLightBar(lightBarInfo, lightBarDetail);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateTMCLightBar(naviTmcInfo);
                }
            }
        }
    }

    /**
     * 区间测速回调，车辆必须经过完整的区间测速路段，并且该路段内有实际限速才会触发
     */
    @Override
    public void onUpdateIntervalCameraDynamicInfo(
            final ArrayList<NaviIntervalCameraDynamicInfo> cameraDynamicList) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            SpeedOverallEntity speedOverallEntity = NaviDataFormatHelper.forMatNaviSpeedCameraInfo(cameraDynamicList);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSpeedOverallInfo(speedOverallEntity);
                }
            }
        }
    }

    /**
     * 绿波车速信息回调，传出绿波车速信息。红绿灯为绿波颜色时，自车位可顺利通过的绿灯的建议车速区间值
     */
    @Override
    public void onUpdateGreenWaveCarSpeed(final ArrayList<NaviGreenWaveCarSpeed> list) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            SpeedOverallEntity speedOverallEntity = NaviDataFormatHelper.forMatNaviGreenWaveInfo(list);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSpeedOverallInfo(speedOverallEntity);
                }
            }
        }
    }

    @Override
    public void onShowNaviCameraExt(final ArrayList<NaviCameraExt> naviCameraList) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            CameraInfoEntity cameraInfo = NaviDataFormatHelper.formatNearestCameraInfo(naviCameraList);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviCameraInfo(cameraInfo);
                }
            }
        }
    }

    /**
     * @param pathID pathId
     * @param result result 1:成功 2:失败，PathId无效 3:失败，因为和当前主选路线一致
     */
    @Override
    public void onSelectMainPathStatus(final long pathID, final int result) {
        Logger.i(TAG, "GuidanceCallback onSelectMainPathStatus: pathID={?}, result={?}", pathID, result);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onSelectMainPathStatus(pathID, result);
                }
            }
        }
    }

    @Override
    public void onUpdateTMCCongestionInfo(final NaviCongestionInfo info) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            final NaviCongestionInfoEntity naviCongestionInfoEntity =  NaviDataFormatHelper.formatNaviCongestionInfo(info);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateTMCCongestionInfo(naviCongestionInfoEntity);
                }
            }
        }
    }

    @Override
    public void onUpdateTrafficLightCountdown(final ArrayList<TrafficLightCountdown> list) {
        if (ConvertUtils.isEmpty(mGuidanceObservers)) {
            return;
        }
        ArrayList<TrafficLightCountdownEntity> trafficLightCountdownEntities = new ArrayList<>();
        if (!ConvertUtils.isEmpty(list)) {
            for (TrafficLightCountdown trafficLightCountdown : list) {
                TrafficLightCountdownEntity entity = NaviDataFormatHelper.
                        formatTrafficLightCountdown(trafficLightCountdown);
                trafficLightCountdownEntities.add(entity);
            }
        }
        for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
            if (guidanceObserver == null) {
                return;
            }
            guidanceObserver.onUpdateTrafficLightCountdown(trafficLightCountdownEntities);
        }
    }

    @Override
    public void onCurrentRoadSpeed(final int speed) {
        Logger.i(TAG, "GuidanceCallback onCurrentRoadSpeed speed：", speed);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onCurrentRoadSpeed(speed);
                }
            }
        }
    }

    @Override
    public void onPlayTTS(final SoundInfo info) {
        if(null == info) {
            Logger.d(TAG, "onPlayTTS info is null");
            return;
        }
        Logger.d(TAG, "onPlayTTS : ",info.text);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            SoundInfoEntity soundInfoEntity =  NaviDataFormatHelper.formatSoundInfo(info);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onPlayTTS(soundInfoEntity);
                }
            }
        }
    }

    @Override
    public void onPlayRing(final int type) {
        Logger.i(TAG, "TTS play type : ", type);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onPlayRing(type);
                }
            }
        }
    }

    /***
     * 因偏航，道路限行，tmc路况拥堵等原因，guide引擎会通知外界进行路线重算
     */
    @Override
    public void onReroute(final RouteOption rerouteOption) {
        if (rerouteOption == null) {
            return;
        }
        Logger.d(TAG, "onReroute: ", "RouteReqId: ", rerouteOption.getRouteReqId(),
                " RouteType: ", rerouteOption.getRouteType());
        if (rerouteOption.getRouteType() == RouteType.RouteTypeYaw) {
            final SoundInfoEntity soundInfo = new SoundInfoEntity();
            String info = ResourceUtils.Companion.getInstance().getString(R.string.route_type_yaw);
            soundInfo.setText(info);
            if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
                for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                    if (guidanceObserver != null) {
                        guidanceObserver.onPlayTTS(soundInfo);
                    }
                }
            }
        }
    }

    /****
     * 透出电动车ETA信息。
     * TODO 模拟导航这里是没有回调的，需要真实环境测试，抓取log分析
     * 透出电动车ETA信息，仅在线支持。一分钟回调一次
     */
    @Override
    public void onUpdateElecVehicleETAInfo(ArrayList<ElecVehicleETAInfo> elecVehicleETAInfo) {
        // 透出电动车ETA信息。透出电动车ETA信息，仅在线支持。一分钟回调一次
        if(ConvertUtils.isEmpty(elecVehicleETAInfo)){
            Logger.i(TAG, "onUpdateElectVehicleETAInfo is null");
            return;
        }
        int VehicleEtaListSize = elecVehicleETAInfo.size();
        Logger.i(TAG, "onUpdateElectVehicleETAInfo", VehicleEtaListSize);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            final List<FyElecVehicleETAInfo> desObj = NaviDataFormatHelper.convertVehicleInfo(elecVehicleETAInfo);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateElectVehicleETAInfo(desObj);
                }
            }
        }
    }

    @Override
    public boolean isPlaying() {
        return NaviAudioPlayer.getInstance().isTTSPlaying();
    }

    @Override
    public void onQueryAppointLanesInfo(final long reqId, final ArrayList<LaneInfo> laneInfo) {
        if (ConvertUtils.isEmpty(laneInfo)) {
            return;
        }
        if (ConvertUtils.isEmpty(mGuidanceObservers)) {
            return;
        }
        ArrayList<LaneInfoEntity> laneInfoList = new ArrayList<>();
        for (int i = 0; i < laneInfo.size(); i++) {
            LaneInfo info = laneInfo.get(i);
            if (ConvertUtils.isEmpty(info)) {
                continue;
            }
            final LaneInfoEntity laneInfoEntity = NaviDataFormatHelper.forMatLaneInfo(info);
            laneInfoList.add(laneInfoEntity);
        }
        for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
            if (guidanceObserver == null) {
                continue;
            }
            guidanceObserver.onLaneInfoReceived(laneInfoList);
        }
    }

    @Override
    public void onShowSameDirectionMixForkInfo(final ArrayList<MixForkInfo> list) {
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        Logger.i(TAG, "MixForkInfo : ", list.size());
        final List<NaviMixForkInfo> formaterMixForkList = NaviDataFormatHelper.formaterMixForkList(list);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onShowSameDirectionMixForkInfo(formaterMixForkList);
                }
            }
        }
    }

    @Override
    public void onShowNaviWeather(ArrayList<NaviWeatherInfo> list) {
        INaviObserver.super.onShowNaviWeather(list);
        Logger.i(TAG, "onShowNaviWeather-Amp");
        if (ConvertUtils.isEmpty(list) || ConvertUtils.isNull(list.get(0)) || ConvertUtils.isEmpty(list.get(0).weatherInfo)) {
            return;
        }
        final WeatherInfo weatherInfo = list.get(0).weatherInfo.get(0);
        if (weatherInfo == null) {
            return;
        }
        final RouteWeatherInfo routeWeatherInfo = new RouteWeatherInfo();
        routeWeatherInfo.setMWeatherID(weatherInfo.weatherID);
        routeWeatherInfo.setMWeatherName(weatherInfo.weatherName);
        routeWeatherInfo.setMAlertLevelName(weatherInfo.alertLevelName);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onShowNaviWeather(routeWeatherInfo);
                }
            }
        }
    }

    @Override
    public void onDeletePath(final ArrayList<Long> pathIDList) {
        Logger.i(TAG, "onDeletePath: ", "经过分歧点");
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onDeletePath(pathIDList);
                }
            }
        }
        onBatterHotCallBack(true);
    }

    @Override
    public void onChangeNaviPath(final long oldPathId, final long pathID) {
        Logger.i(TAG, "onChangeNaviPath: ", "切换路线");
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onChangeNaviPath(oldPathId, pathID);
                }
            }
        }
    }

    @Override
    public void onSuggestChangePath(long newPathID, long oldPathID,
                                    SuggestChangePathReason reason) {
        long saveTime = reason == null ? 0 : reason.saveTime;
        Logger.i(TAG, "onSuggestChangePath: ", "建议切换路线 newPathId = ", newPathID,
                " oldPathId = ", oldPathID, " saveTime = ", saveTime);
        SuggestChangePathReasonEntity suggestChangePathReasonEntity =
                new SuggestChangePathReasonEntity();
        suggestChangePathReasonEntity.setSaveTime(saveTime);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onSuggestChangePath(newPathID, oldPathID,
                            suggestChangePathReasonEntity);
                }
            }
        }
    }

    @Override
    public void onUpdateTREvent(ArrayList<PathTrafficEventInfo> pathsTrafficEventInfo,
                                long pathCount) {
        Logger.i(TAG, "onUpdateTREvent: ", "更新路线交通事件");
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    // 暂留，交通事件回调
//                    guidanceObserver.onUpdateTREvent(pathsTrafficEventInfo, pathCount);
                }
            }
        }
    }

    @Override
    public void onShowNaviFacility(ArrayList<NaviRoadFacility> list) {
        Logger.i(TAG, "onShowNaviFacility: ", "引导道路设施回调");
        ArrayList<NaviRoadFacilityEntity> naviRoadFacilityEntities = NaviDataFormatHelper.
                formatRoadFacilityList(list);
        if (ConvertUtils.isEmpty(naviRoadFacilityEntities)) {
            Logger.i(TAG, "onShowNaviFacility: ", "道路设施列表为空");
            return;
        }
        for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
            if (guidanceObserver != null) {
                guidanceObserver.onShowNaviFacility(naviRoadFacilityEntities);
            }
        }
    }

    @Override
    public void onDriveReport(DriveReport report) {
        Logger.i(TAG, "onDriveReport");
        if (report != null && report.blNaviStatisticsInfo != null) {
            NaviDriveReportEntity naviDriveReportEntity = new NaviDriveReportEntity();
            naviDriveReportEntity.setDrivenTime(report.blNaviStatisticsInfo.drivenTime);
            naviDriveReportEntity.setDrivenDist(report.blNaviStatisticsInfo.drivenDist);
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onDriveReport(naviDriveReportEntity);
                }
            }
        }
    }
}