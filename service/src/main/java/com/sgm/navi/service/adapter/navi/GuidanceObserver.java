package com.sgm.navi.service.adapter.navi;

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
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navi.SuggestChangePathReasonEntity;
import com.sgm.navi.service.define.navi.TrafficLightCountdownEntity;
import com.sgm.navi.service.define.route.FyRouteOption;
import com.sgm.navi.service.define.route.RouteWeatherInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * 导航相关监听
 *
 * @author sgm
 * @version $Revision.*$
 */
public interface GuidanceObserver extends BaseNaviObserver {

    default void onNaviStart() {

    }

    /**
     * 传出当前导航信息
     *
     * @param naviETAInfo naviETAInfo
     */
    default void onNaviInfo(NaviEtaInfo naviETAInfo) {

    }

    /**
     * 车道线信息
     *
     * @param isShowLane     isShowLane
     * @param laneInfoEntity laneInfoEntity
     */
    default void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {

    }

    /**
     * 路口大图信息
     *
     * @param isShowImage   isShowImage
     * @param naviImageInfo naviImageInfo
     */
    default void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {

    }

    /**
     * 导航到达目的地
     *
     * @param traceId  traceId
     * @param naviType naviType
     */
    default void onNaviArrive(long traceId, int naviType) {
    }

    /**
     * 转向图标信息、以及传出出入口信息
     *
     * @param respData respData
     */
    default void onManeuverInfo(NaviManeuverInfo respData) {
    }

    /**
     * 路况条信息
     *
     * @param naviTmcInfo naviTmcInfo
     */
    default void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
    }

    /**
     * 区间车速、绿波车速
     *
     * @param speedEntity speedEntity
     */
    default void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity) {
    }

    /**
     * 电子眼信息
     *
     * @param cameraInfo cameraInfo
     */
    default void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
    }

    /**
     * 服务区、收费站信息、收费口车道类型信息
     *
     * @param sapaInfoEntity sapaInfoEntity
     */
    default void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
    }

    /**
     * 经过途径点
     *
     * @param viaIndex 途经点索引
     */
    default void onUpdateViaPass(long viaIndex) {
    }

    /**
     * 电池预加热
     *
     * @param isPass 是否是通过的点
     */
    default void onBatterHotCallBack(boolean isPass) {
    }

    /**
     * 通知用户切换主导航路线状态，客户端主动SelectMainPathID切换的回调状态
     *
     * @param pathID pathID
     * @param result result
     */
    default void onSelectMainPathStatus(long pathID, int result) {
    }

    /**
     * 更新播报模式
     *
     * @param broadcastType broadcastType
     * @param isDay         isDay
     */
    default void updateBroadcastParam(int broadcastType, boolean isDay) {
    }

    /**
     * 当前道路限速设施的限速 0: 隐藏/不显示限速  !=0: 显示当前道路限速设施的限速
     *
     * @param speed 车速
     */
    default void onCurrentRoadSpeed(int speed) {
    }

    /***
     * 透出电动车ETA信息
     * 透出电动车ETA信息，仅在线支持。一分钟回调一次
     * @param infos infos
     */
    default void onUpdateElectVehicleETAInfo(List<FyElecVehicleETAInfo> infos) {
    }

    /**
     * 下一条道路的车道线信息.
     *
     * @param laneInfoList 车道线实体
     */
    default void onLaneInfoReceived(ArrayList<LaneInfoEntity> laneInfoList) {
    }

    /**
     * 下个路口的红绿灯信息.
     *
     * @param isHaveTrafficLight 红绿灯实体
     * @param geoPoint           红绿灯的位置
     */
    default void onUpdateTrafficLightCountdown(final ArrayList<TrafficLightCountdownEntity> list) {
    }

    /**
     * 剩余道路的混淆路口信息.
     *
     * @param list 混淆路口集合
     */
    default void onShowSameDirectionMixForkInfo(List<NaviMixForkInfo> list) {
    }

    /**
     * @param report 驾驶信息
     */
    default void onDriveReport(NaviDriveReportEntity report) {

    }

    /**
     * @param pathIDList 路线ID
     */
    default void onDeletePath(ArrayList<Long> pathIDList) {

    }

    /***
     * 重新算路
     * @param routeOption
     */
    default void onReroute(final FyRouteOption routeOption) {
    }

    /**
     * 经过充电站
     *
     * @param viaIndex
     */
    default void onUpdateChargeStationPass(final long viaIndex) {
    }

    /***
     * 导航中天气信息透出
     * @param info
     */
    default void onShowNaviWeather(final RouteWeatherInfo info) {
    }

    /**
     * 路线改变的回调
     *
     * @param oldPathId 老得路线ID
     * @param pathID    新的路线ID
     */
    default void onChangeNaviPath(final long oldPathId, final long pathID) {

    }

    /**
     * 引导切换到备选路线的回调
     *
     * @param newPathID 新路线id
     * @param oldPathID 旧路线id
     * @param reason    原因
     */
    default void onSuggestChangePath(long newPathID, long oldPathID,
                                     SuggestChangePathReasonEntity reason) {

    }

    default void onShowNaviFacility(ArrayList<NaviRoadFacilityEntity> naviRoadFacilityEntity) {

    }

    default void onUpdateTMCCongestionInfo(NaviCongestionInfoEntity naviCongestionInfoEntity) {

    }
}
