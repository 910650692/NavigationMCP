package com.fy.navi.service.adapter.navi;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviMixForkInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;

import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface GuidanceObserver extends BaseNaviObserver {
    /*传出当前导航信息*/
    default void onNaviInfo(NaviEtaInfo naviETAInfo){
        
    }

    /*车道线信息*/
    default void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity){
        
    }

    /*路口大图信息*/
    default void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo){
        
    }

    /*导航到达目的地*/
    default void onNaviArrive(long traceId, int naviType){}

    /*转向图标信息、以及传出出入口信息*/
    default void onManeuverInfo(NaviManeuverInfo respData){}

    /*路况条信息*/
    default  void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo){}

    /*区间车速、绿波车速*/
    default void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity){}

    /*电子眼信息*/
    default void onNaviCameraInfo(CameraInfoEntity cameraInfo){}

    /*服务区、收费站信息、收费口车道类型信息*/
    default void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity){}

    /*经过途径点*/
    default  void onUpdateViaPass(long viaIndex){}

    /*通知用户切换主导航路线状态，客户端主动SelectMainPathID切换的回调状态*/
    default  void onSelectMainPathStatus(long pathID, int result){}

    /*更新播报模式*/
    default  void updateBroadcastParam(int broadcastType, boolean isDay){}

    /*当前道路限速设施的限速 0: 隐藏/不显示限速  !=0: 显示当前道路限速设施的限速*/
    default void onCurrentRoadSpeed(int speed){}

    /***
     * 透出电动车ETA信息
     * 透出电动车ETA信息，仅在线支持。一分钟回调一次
     */
    default void onUpdateElecVehicleETAInfo(List<FyElecVehicleETAInfo> infos){}

    default void onLaneInfoReceived(LaneInfoEntity laneInfoEntity){}

    default void onUpdateTrafficLightCountdown(int isHaveTrafficLight, GeoPoint geoPoint){}

    default void onShowSameDirectionMixForkInfo(List<NaviMixForkInfo> list){}
}
