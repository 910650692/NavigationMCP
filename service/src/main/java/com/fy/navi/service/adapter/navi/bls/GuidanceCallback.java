package com.fy.navi.service.adapter.navi.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.path.model.ElecVehicleETAInfo;
import com.autonavi.gbl.common.path.model.TollGateInfo;
import com.autonavi.gbl.common.path.option.RouteOption;
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
import com.autonavi.gbl.guide.model.SAPAInquireResponseData;
import com.autonavi.gbl.guide.model.SoundInfo;
import com.autonavi.gbl.guide.model.TrafficLightCountdown;
import com.autonavi.gbl.guide.observer.INaviObserver;
import com.autonavi.gbl.guide.observer.ISoundPlayObserver;
import com.autonavi.gbl.util.model.BinaryStream;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviMixForkInfo;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * 导航信息观察者
 * @author fy
 * @version $Revision.*$
 */
public class GuidanceCallback implements INaviObserver, ISoundPlayObserver {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private Hashtable<String, GuidanceObserver> mGuidanceObservers;

    public GuidanceCallback(final Hashtable<String, GuidanceObserver> guidanceObservers) {
        this.mGuidanceObservers = guidanceObservers;
    }

    /**
     * 一秒回调一次，返回所有可选线路的信息（条条大路通罗马）
     *
     * @param naviInfoList 导航信息列表
     */
    @Override
    public void onUpdateNaviInfo(final ArrayList<NaviInfo> naviInfoList) {
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
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onManeuverInfo(NaviDataFormatHelper.formatManeuverInfo(info));
                }
            }
        }
    }

    @Override
    public void onObtainManeuverIconData(final ManeuverIconResponseData respData) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onManeuverInfo(NaviDataFormatHelper.formatManeuverIconData(respData));
                }
            }
        }
    }

    @Override
    public void onUpdateExitDirectionInfo(final ExitDirectionInfo boardInfo) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onManeuverInfo(NaviDataFormatHelper.formatExitDirectionInfo(boardInfo));
                }
            }
        }
    }

    @Override
    public void onShowTollGateLane(final TollGateInfo tollGateInfo) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSAPAInfo(NaviDataFormatHelper.formatTollGateInfo(tollGateInfo));
                }
            }
        }
    }

    @Override
    public void onShowCrossImage(final CrossImageInfo info) {
        final CrossImageEntity naviImageInfo = NaviDataFormatHelper.forMatImageInfo(info);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onCrossImageInfo(naviImageInfo != null, naviImageInfo);
                }
            }
        }
    }

    @Override
    public void onHideCrossImage(final int type) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onCrossImageInfo(false, NaviDataFormatHelper.forMatImageInfo(type));
                }
            }
        }
    }

    @Override
    public void onShowNaviCrossTMC(final BinaryStream dataBuf) {
        INaviObserver.super.onShowNaviCrossTMC(dataBuf);
    }

    @Override
    public void onPassLast3DSegment() {
        INaviObserver.super.onPassLast3DSegment();
    }

    @Override
    public void onShowNaviLaneInfo(final LaneInfo info) {
        final LaneInfoEntity laneInfoEntity = NaviDataFormatHelper.forMatLaneInfo(info);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onLaneInfo(!ConvertUtils.isEmpty(laneInfoEntity), laneInfoEntity);
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
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSAPAInfo(NaviDataFormatHelper.forMatSAPAInfo(serviceAreaList));
                }
            }
        }
    }

    @Override
    public void onObtainSAPAInfo(final SAPAInquireResponseData respData) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSAPAInfo(NaviDataFormatHelper.forMatSAPAInfo(respData));
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

    @Override
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
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateTMCLightBar(NaviDataFormatHelper.forMatTMCLightBar(lightBarInfo, lightBarDetail));
                }
            }
        }
    }

    @Override
    public void onUpdateIntervalCameraDynamicInfo(
            final ArrayList<NaviIntervalCameraDynamicInfo> cameraDynamicList) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSpeedOverallInfo(
                            NaviDataFormatHelper.forMatNaviSpeedCameraInfo(cameraDynamicList));
                }
            }
        }
    }

    @Override
    public void onUpdateGreenWaveCarSpeed(final ArrayList<NaviGreenWaveCarSpeed> list) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviSpeedOverallInfo(NaviDataFormatHelper.forMatNaviGreenWaveInfo(list));
                }
            }
        }
    }

    @Override
    public void onShowNaviCameraExt(final ArrayList<NaviCameraExt> naviCameraList) {
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onNaviCameraInfo(NaviDataFormatHelper.formatNearestCameraInfo(naviCameraList));
                }
            }
        }
    }

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
    }

    @Override
    public void onUpdateTrafficLightCountdown(final ArrayList<TrafficLightCountdown> list) {
        if (ConvertUtils.isEmpty(mGuidanceObservers)) {
            return;
        }
        int num = 0;
        if (!ConvertUtils.isEmpty(list)) {
            num = list.size();
        }
        final GeoPoint geoPoint = new GeoPoint();
        if (!ConvertUtils.isEmpty(list)) {
            final TrafficLightCountdown lightCountdown = list.get(0);
            final Coord2DDouble coord2DDouble = lightCountdown.position;
            geoPoint.setLat(coord2DDouble.lat);
            geoPoint.setLon(coord2DDouble.lon);
        }
        for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
            if (guidanceObserver == null) {
                return;
            }
            guidanceObserver.onUpdateTrafficLightCountdown(num, geoPoint);
        }
    }

    @Override
    public void onCurrentRoadSpeed(final int speed) {
        Logger.i(TAG, "GuidanceCallback onCurrentRoadSpeed speed：" + speed);
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
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onPlayTTS(NaviDataFormatHelper.formatSoundInfo(info));
                }
            }
        }
    }

    @Override
    public void onPlayRing(final int type) {
        Logger.i(TAG, "TTS play type : " + type);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onPlayTTS(NaviDataFormatHelper.formatSoundInfo(type));
                }
            }
        }
    }

    @Override
    public void onReroute(final RouteOption rerouteOption) {
        Logger.i(TAG, "GuidanceCallback onReroute:");
        // TODO: 2025/1/6 此处需要触发算路
    }

    @Override
    public void onUpdateElecVehicleETAInfo(final ArrayList<ElecVehicleETAInfo> elecVehicleETAInfo) {
        INaviObserver.super.onUpdateElecVehicleETAInfo(elecVehicleETAInfo);
        Logger.i(TAG, "onUpdateElecVehicleETAInfo:" + (elecVehicleETAInfo == null));
        // 透出电动车ETA信息。透出电动车ETA信息，仅在线支持。一分钟回调一次
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onUpdateElecVehicleETAInfo(
                            GsonUtils.fromJson2List(elecVehicleETAInfo, FyElecVehicleETAInfo.class)
                    );
                }
            }
        }
    }

    @Override
    public boolean isPlaying() {
        return false;
    }

    @Override
    public void onQueryAppointLanesInfo(final long reqId, final ArrayList<LaneInfo> laneInfo) {
        if (ConvertUtils.isEmpty(laneInfo)) {
            return;
        }
        if (ConvertUtils.isEmpty(mGuidanceObservers)) {
            return;
        }
        final LaneInfo laneInfo1 = laneInfo.get(0);
        if (ConvertUtils.isEmpty(laneInfo1)) {
            return;
        }
        final LaneInfoEntity laneInfoEntity = NaviDataFormatHelper.forMatLaneInfo(laneInfo1);
        for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
            if (guidanceObserver == null) {
                continue;
            }
            guidanceObserver.onLaneInfoReceived(laneInfoEntity);
        }
    }

    @Override
    public void onShowSameDirectionMixForkInfo(final ArrayList<MixForkInfo> list) {
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        Logger.i(TAG, "MixForkInfo : " + list.size());
        final List<NaviMixForkInfo> formaterMixForkList = NaviDataFormatHelper.formaterMixForkList(list);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onShowSameDirectionMixForkInfo(formaterMixForkList);
                }
            }
        }
    }

    /**
     * @param report 驾驶报告
     */
    @Override
    public void onDriveReport(final DriveReport report) {
        if (ConvertUtils.isEmpty(report)) {
            return;
        }
        final NaviDriveReportEntity naviDriveReportEntity = NaviDataFormatHelper.
                formaterDriveReport(report);
        if (!ConvertUtils.isEmpty(mGuidanceObservers)) {
            for (GuidanceObserver guidanceObserver : mGuidanceObservers.values()) {
                if (guidanceObserver != null) {
                    guidanceObserver.onDriveReport(naviDriveReportEntity);
                }
            }
        }
    }
}
