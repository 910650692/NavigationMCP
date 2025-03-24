package com.fy.navi.service.logicpaket.navi;

import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface IGuidanceObserver {
    /*传出当前导航信息*/
    default void onNaviInfo(NaviEtaInfo naviETAInfo) {}

    /*车道线信息*/
    default void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {}

    /*路口大图信息*/
    default void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {}

    /*导航到达目的地*/
    default void onNaviArrive(long traceId, int naviType) {}

    /*导航结束*/
    default void onNaviStop() {}

    /*转向图标信息、以及传出出入口信息*/
    default void onManeuverInfo(NaviManeuverInfo respData) {}

    /*路况条信息*/
    default void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {}

    /*区间车速、绿波车速*/
    default void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity) {}

    /*电子眼信息*/
    default void onNaviCameraInfo(CameraInfoEntity cameraInfo) {}

    /*服务区、收费站信息、收费口车道类型信息*/
    default void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {}

    /*经过途径点*/
    default void onUpdateViaPass(long viaIndex) {}

    /*通知用户切换主导航路线状态，客户端主动SelectMainPathID切换的回调状态*/
    default void onSelectMainPathStatus(long pathID, int result) {}

    /*当前道路限速设施的限速 0: 隐藏/不显示限速  !=0: 显示当前道路限速设施的限速*/
    default void onCurrentRoadSpeed(int speed) {}

    /*语音打开/关闭路线全览，需要HMI模拟全览/退出全览点击操作*/
    default void onVoiceOverview(MapTypeId mapTypeId, boolean open) {}

    /*语音切换主辅路、高架上下*/
    default void onVoiceParallelOption(MapTypeId mapTypeId, String parallelOption) {}

    /*语音继续导航*/
    default void onVoiceContinueNavigation(MapTypeId mapTypeId) {}

    default void onDriveReport(NaviDriveReportEntity naviDriveReportEntity) {}
}