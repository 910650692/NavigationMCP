package com.fy.navi.service.adapter.route;

import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteL2Data;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteTMCParam;
import com.fy.navi.service.define.route.RouteTrafficIncidentParam;
import com.fy.navi.service.define.route.RouteWeatherParam;

import java.util.ArrayList;

/**
 * @author lvww
 * @version  \$Revision.1.0\$
 * description TODO
 * date 2024/12/5
 */
public interface RouteResultObserver {

    /**
     * 算路成功回调
     * @param successMsg 算路成功信息
     */
    void onRouteSuccess(String successMsg);

    /**
     * 路段聚合信息及路线详情
     * @param requestRouteResult 路线结果类
     */
    void onRouteResult(RequestRouteResult requestRouteResult);

    /**
     * 路线绘制回调
     * @param routeLineLayerParam 路线绘制参数
     */
    void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam);

    /**
     * 路线上服务区数据回调
     * @param routeRestAreaParam 服务区数据
     */
    void onRouteRestAreaInfo(RouteRestAreaParam routeRestAreaParam);

    /**
     * 路线上天气数据回调
     * @param routeWeatherParam 天气总数据
     */
    void onRouteWeatherInfo(RouteWeatherParam routeWeatherParam);

    /**
     * 路线上充电站数据回调
     * @param routeChargeStationParam 充电站信息
     */
    void onRouteChargeStationInfo(RouteChargeStationParam routeChargeStationParam);

    /**
     * 路线上替换充电站数据回调
     * @param routeAlterChargeStationParam 备选充电桩信息
     */
    void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam);

    /**
     * 限行提示
     * @param routeRestrictionParam 限行总数据
     */
    void onRouteRestrictionInfo(RouteRestrictionParam routeRestrictionParam);

    /**
     * 沿途收费站
     * @param routeRestTollGateParam 收费站数据
     */
    void onRouteRestTollGateInfo(RouteRestTollGateParam routeRestTollGateParam);

    /**
     * 沿途城市列表
     * @param routeAlongCityParam 城市列表数据
     */
    void onRouteCityInfo(RouteAlongCityParam routeAlongCityParam);

    /**
     * 沿途交通事件
     * @param routeTrafficIncidentParam 交通详情数据
     */
    void onRouteTrafficIncidentInfo(RouteTrafficIncidentParam routeTrafficIncidentParam);

    /**
     * 续航里程信息
     * @param evRangeOnRouteInfos 能量耗尽点信息集合
     */
    void onRouteRanges(ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos);

    /**
     * 算路失败回调
     * @param requestRouteResult 路线结果类
     * @param errorCode 错误码
     * @param errorMsg 失败信息
     * @param requestId 请求id
     */
    void onRouteFail(RequestRouteResult requestRouteResult, int errorCode, String errorMsg, long requestId);

    /**
     * 路线上充电站数据回调    、
     * @param routeL2Data 路线信息
     */
    void onRouteL2Info(RouteL2Data routeL2Data);

    /**
     * 通勤模式TMC信息   、
     * @param param TMC
     */
    void onRouteTMCInfo(RouteTMCParam param);

    /**
     * 静默算路消息
     */
    void onReRoute();
}
