package com.fy.navi.service.logicpaket.navi;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviRoadFacilityEntity;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navi.SuggestChangePathReasonEntity;
import com.fy.navi.service.define.route.FyRouteOption;
import com.fy.navi.service.define.route.RouteWeatherInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * @author fy
 * @version $Revision.*$
 */
public interface IGuidanceObserver {
    /**
     * @param naviETAInfo 导航信息
     */
    default void onNaviInfo(NaviEtaInfo naviETAInfo) {

    }

    /**
     * @param isShowLane 是否显示车道线
     * @param laneInfoEntity 车道线信息
     */
    default void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {

    }

    /**
     * @param isShowImage 是否显示大图
     * @param naviImageInfo 路口大图信息
     */
    default void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {

    }

    /**
     * 导航到达目的地
     * @param traceId traceId
     * @param naviType 导航类型
     */
    default void onNaviArrive(long traceId, int naviType) {

    }

    /**
     * 导航结束
     */
    default void onNaviStop() {

    }

    /**
     * @param respData 转向图标信息
     */
    default void onManeuverInfo(NaviManeuverInfo respData) {

    }

    /**
     * @param naviTmcInfo 路况条信息
     */
    default void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {

    }

    /**
     * @param speedEntity 区间车速、绿波车速
     */
    default void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity) {

    }

    /**
     * @param cameraInfo 电子眼信息
     */
    default void onNaviCameraInfo(CameraInfoEntity cameraInfo) {

    }

    /**
     * 服务区、收费站信息、收费口车道类型信息
     * @param sapaInfoEntity 服务区、收费站信息、收费口车道类型信息
     */
    default void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {

    }

    /**
     * 经过途径点
     * @param viaIndex 经过途径点
     */
    default void onUpdateViaPass(long viaIndex) {

    }

    /**
     * 通知用户切换主导航路线状态，客户端主动SelectMainPathID切换的回调状态
     * @param pathID 主导航路线ID
     * @param result 状态
     */
    default void onSelectMainPathStatus(long pathID, int result) {

    }

    /**
     * 当前道路限速设施的限速 0: 隐藏/不显示限速  !=0: 显示当前道路限速设施的限速
     * @param speed 当前道路限速设施的限速
     */
    default void onCurrentRoadSpeed(int speed) {

    }

    /**
     * 语音打开/关闭路线全览，需要HMI模拟全览/退出全览点击操作
     * @param mapTypeId 屏幕id
     * @param open 是否开启全览
     */
    default void onVoiceOverview(MapType mapTypeId, boolean open) {

    }

    /**
     * 语音切换主辅路、高架上下
     * @param mapTypeId 屏幕id
     * @param parallelOption 平行路切换操作
     */
    default void onVoiceParallelOption(MapType mapTypeId, String parallelOption) {

    }

    /**
     * 语音继续导航
     * @param mapTypeId 屏幕id
     */
    default void onVoiceContinueNavigation(MapType mapTypeId) {

    }

    /**
     * @param naviDriveReportEntity 驾驶行为报告实体
     */
    default void onDriveReport(NaviDriveReportEntity naviDriveReportEntity) {

    }

    /***
     * 重新算路
     * @param routeOption
     */
    default void onReroute(FyRouteOption routeOption) {

    }

    /***
     * 电动车Eta透出
     * @param infos
     */
    default void onUpdateElectVehicleETAInfo(List<FyElecVehicleETAInfo> infos) {

    }

    default void onUpdateTrafficLightCountdown(int isHaveTrafficLight, GeoPoint geoPoint) {

    }

    /***
     * 途径充电桩回调
     * @param viaIndex
     */
    default void onUpdateChargeStationPass(long viaIndex) {

    }

    /***
     * 导航中天气信息透出
     * @param info
     */
    default void onShowNaviWeather(final RouteWeatherInfo info) {
    }

    /**
     * 路线切换
     * @param oldPathId 旧路线
     * @param pathID 新路线
     */
    default void onChangeNaviPath(long oldPathId, long pathID) {

    }

    /**
     * 更优路线回调
     * @param newPathID 新道路id
     * @param oldPathID 旧道路id
     * @param reason 原因
     */
    default void onSuggestChangePath(long newPathID, long oldPathID,
                                     SuggestChangePathReasonEntity reason) {

    }

    /**
     * 导航设施信息回调
     * @param naviRoadFacilityEntity 导航设施信息
     */
    default void onShowNaviFacility(ArrayList<NaviRoadFacilityEntity> naviRoadFacilityEntity) {

    }
}