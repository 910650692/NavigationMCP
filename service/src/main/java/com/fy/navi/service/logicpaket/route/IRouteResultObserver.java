package com.fy.navi.service.logicpaket.route;

import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteL2Data;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteTMCParam;
import com.fy.navi.service.define.route.RouteTrafficIncidentParam;
import com.fy.navi.service.define.route.RouteWeatherParam;

import java.util.ArrayList;
import java.util.List;

/**
 * description TODO
 * @author lvww
 * @version  \$Revision.1.0\$
 * date 2024/12/5
 */
public interface IRouteResultObserver {

    /**
     * 算路成功回调
     * @param successMsg 算路成功信息
     */
    default void onRouteSuccess(String successMsg) {
    }

    /**
     * 路段聚合信息及路线详情
     * @param requestRouteResult 路线结果类
     */
    default void onRouteResult(RequestRouteResult requestRouteResult) {
    }

    /**
     * 路线绘制回调
     * @param routeLineLayerParam 路线绘制参数
     */
    default void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
    }

    /**
     * 路线上服务区数据回调
     * @param routeRestAreaParam 服务区数据
     */
    default void onRouteRestAreaInfo(RouteRestAreaParam routeRestAreaParam) {
    }

    /**
     * 路线上天气数据回调
     * @param routeWeatherParam 天气总数据
     */
    default void onRouteWeatherInfo(RouteWeatherParam routeWeatherParam) {
    }

    /**
     * 路线上充电站数据回调
     * @param routeChargeStationParam 充电站信息
     */
    default void onRouteChargeStationInfo(RouteChargeStationParam routeChargeStationParam) {
    }

    /**
     * 路线上替换充电站数据回调
     * @param routeAlterChargeStationParam 备选充电桩信息
     */
    default void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {
    }

    /**
     * 限行提示
     * @param routeRestrictionParam 限行总数据
     */
    default void onRouteRestrictionInfo(RouteRestrictionParam routeRestrictionParam) {
    }

    /**
     * 限行区域提示
     * @param routeRestrictionParam 限行总数据
     */
    default void onRouteDrawReStrictedAreaInfo(RouteRestrictionParam routeRestrictionParam) {
    }

    /**
     * 沿途收费站
     * @param routeRestTollGateParam 收费站数据
     */
    default void onRouteRestTollGateInfo(RouteRestTollGateParam routeRestTollGateParam) {
    }

    /**
     * 沿途城市列表
     * @param routeAlongCityParam 城市列表数据
     */
    default void onRouteCityInfo(RouteAlongCityParam routeAlongCityParam) {
    }

    /**
     * 沿途交通事件
     * @param routeTrafficIncidentParam 交通详情数据
     */
    default void onRouteTrafficIncidentInfo(RouteTrafficIncidentParam routeTrafficIncidentParam) {
    }

    /**
     * 算路成功所有点信息回调
     * @param requestRouteResult 路线结果类
     */
    default void onRouteAllRoutePoiInfo(RequestRouteResult requestRouteResult) {
    }

    /**
     * 算路失败回调
     * @param mapTypeId 屏幕id
     * @param errorMsg 失败信息
     */
    default void onRouteFail(MapType mapTypeId, String errorMsg) {
    }

    /**
     * 开始离线算路回调
     * @param mapTypeId 屏幕id
     * @param errorMsg 失败信息
     */
    default void onRouteOffline(MapType mapTypeId, String errorMsg) {
    }

    /**
     * 续航里程信息
     * @param evRangeOnRouteInfos 能量耗尽点信息集合
     */
    default void onRouteRanges(ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {
    }

    /**
     * 基本算路请求
     */
    default void onRouteRequest() {
    }

    /**
     * 切换路线      、
     * @param mapTypeId 屏幕id
     * @param routeIndex 路线id
     */
    default void onRouteSlected(MapType mapTypeId, int routeIndex, boolean isFirst) {
    }

    /**
     * 路线上充电站数据回调    、
     * @param routeL2Data 路线信息
     */
    default void onL2DataCallBack(RouteL2Data routeL2Data) {

    }

    /**
     * 添加的途经点数量提供给语音
     * @param size 已添加的途经点数量
     */
    default void onSpeechViaNum(int size) {

    }

    /**
     * 终点城市名提供给语音
     * @param cityName 城市名
     * @param endName 终点名称
     */
    default void onSpeechEndCityName(String cityName, String endName) {

    }

    /**
     * 通勤模式TMC信息   、
     * @param param TMC信息
     */
    default void onRouteTMCInfo(RouteTMCParam param) {

    }

    /**
     * 静默算路消息
     */
    default void onReroute() {

    }


    /**
     * 静默算路失败
     */
    default void onReRouteError() {

    }

    default void onRouteDetails(List<RouteLineSegmentInfo> routeLineDetail) {

    }
}
