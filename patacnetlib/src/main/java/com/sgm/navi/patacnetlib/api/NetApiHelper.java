package com.sgm.navi.patacnetlib.api;

public interface NetApiHelper {

    String ACTIVATE_TAG = "NaviApp_Activate_Service";

    String APP_KEY_TAG = "appKey";
    String UUID_TAG = "uuid";
    String CREATE_ORDER_TAG = "createOrder";
    String QUERY_ORDER_TAG = "queryOrder";


//    String DOMAIN = "test-ninfo-securitygateway.sgmlink.com";
    String DOMAIN = "ninfo-securitygateway.sgmlink.com";

    // =============================================

    //appKey请求
    String QUERY_APP_KEY = "/info4gw/apigateway/public/generateAppKey/";

    //获取uuid
    String QUERY_UUID = "/info4gw/apigateway/public/vehicle/encrypt-vin";

    //下单
    String CREATE_ORDER = "/info4gw/apigateway/info4-map/sgmMap/createOrder";

    //查单
    String QUERY_ORDER = "/info4gw/apigateway/info4-map/sgmMap/orderQuery";


    // ============================================

    // 充电站列表查询
    String CLOUD_QUERY_STATION_NEW = "/info4gw/apigateway/info4-map/charging-station/csds/queryStationNew";
    // 充电站收藏查询
    String CLOUD_QUERY_COLLECT_STATION = "/info4gw/apigateway/info4-map/charging-station/saving/list";
    // 修改预约
    String UPDATE_RESERVATION = "/info4gw/apigateway/info4-map/charging-station/npo/charge/update";
    // 创建公桩预约
    String CREATE_RESERVATION_STATION = "/info4gw/apigateway/info4-map/charging-station/npo/pre/create";
    // 开启地锁
    String CLOUD_QUERY_UNLOCK = "/info4gw/apigateway/info4-map/charging-station/csds/chargeStation/unLock";
    // 更新用户收藏充电站
    String UPDATE_COLLECT_STATION = "/info4gw/apigateway/info4-map/charging-station/saving/update";
    // 查询公桩预约
    String QUERY_RESERVATION_STATION = "/info4gw/apigateway/info4-map/charging-station/npo/pre/query";
    // 获取充电桩信息
    String QUERY_EQUIPMENT_INFO = "/info4gw/apigateway/info4-map/charging-station/csds/queryStation/equipment";
    // 获取充电站详情
    String QUERY_STATION_INFO = "/info4gw/apigateway/info4-map/charging-station/csds/queryStation/info";

}
