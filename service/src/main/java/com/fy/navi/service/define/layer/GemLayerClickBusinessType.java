package com.fy.navi.service.define.layer;

/**
 * Author: QiuYaWei
 * Date: 2025/2/10
 * Description: [在这里描述文件功能]
 */
public enum GemLayerClickBusinessType {
    // TODO 自定义扎标和点击响应，包括图片和文字

    // 搜索扎标点击
    BizSearchTypePoiParentPoint,
    BizSearchTypePoiChildPoint,
    BizSearchTypePoiParkRoute,
    BizSearchTypeChargeStation,

    //路线上图层点击
    BizRouteTypeStartPoint,
    BizRouteTypeEndPoint,
    BizRouteTypeViaPoint,
    BizRouteTypeWeather,
    BizRouteTypeRestArea,
    BizRouteTypeViaChargeStationPoint,
    BizRouteTypeTrafficEventTip,
    BizRouteTypePath,
    BizRouteTypeJamPoint,

    //Label图层点击
    BizLabelTypeRoutePopSearchPoint,    //终点可停车
    // 未定义
    UnKnown

}
