package com.sgm.navi.service.adapter.route;

import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.route.RouteAvoidInfo;
import com.sgm.navi.service.define.route.RouteCurrentPathParam;
import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePreferenceID;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.math.BigInteger;
import java.util.List;

/**
 * @author lvww
 * @version  \$Revision.1.0\$
 * description TODO
 * date 2024/12/5
 */
public interface IRouteApi {

    /**
     * 算路服务初始化
     */
    void initRouteService();

    /**
     * 注册算路回调监听
     * @param key key
     * @param routeResultObserver 回调监听对象
     */
    void registerRouteObserver(String key, RouteResultObserver routeResultObserver);

    /**
     * 设置路线偏好
     * @param routePreferenceID 偏好Id
     */
    void setRoutePreference(RoutePreferenceID routePreferenceID);

    /**
     * 算路请求
     *
     * @param param 算路参数
     * @param paramList 算路设置参数集合
     * @return 返回请求taskId
     */
    long requestRoute(RouteRequestParam param, List<RouteParam> paramList);

    /**
     * 请求路线上的天气
     *
     * @param routeLineLayerParam 路线参数
     * @param index 路线ID
     * @return 返回请求的taskId
     */
    long requestRouteWeather(RouteLineLayerParam routeLineLayerParam, int index);

    /**
     * 请求备选充电站
     *
     * @param pathInfo 路线参数
     * @param poiId 推荐充电站
     * @return 返回请求的taskId
     */
    long requestRouteAlternativeChargeStation(Object pathInfo, String poiId);

    /**
     * 放弃算路
     *
     * @param requestId 请求的taskId
     * @return 返回是否成功
     */
    boolean cancelRoute(long requestId);

    /**
     * 解注册算路回调监听
     * @param key key
     */
    void removeRouteObserver(String key);

    /**
     * 反初始化算路服务
     */
    void unInitRouteService();

    /**
     * 设置避开道路
     * @param routeAvoidInfo 避开参数
     */
    void setAvoidRoad(RouteAvoidInfo routeAvoidInfo);

    /**
     * 设置请求控制
     * @param id 偏好
     * @param num 车牌
     * @param restriction 是否避开实现
     * @param routePlan 引导是否激活
     */
    void setRequestControl(RoutePreferenceID id, String num, boolean restriction, boolean routePlan);

    /**
     * 设置终点
     * @param poiInfoEntity 终点信息
     */
    void sendEndEntity(PoiInfoEntity poiInfoEntity);

    /**
     * 路线还原，手机算路
     * @param routeMsgPushInfo 算路还原参数
     * @param mapTypeId 屏幕Id
     */
    void requestRouteRestoration(RouteMsgPushInfo routeMsgPushInfo, MapType mapTypeId);

    /**
     * 路线还原，手机算路
     * @param requestId 算路还原参数
     */
    void abortRequest(long requestId);

    /**
     * 平行路切换完成重算路
     *
     * @param switchRoadType 切换类型
     * @param locInfoBean    定位
     * @param roadID         路线ID
     * @param flag           主辅路状态
     * @param hwFlag         高架桥状态
     */
    long requestSwitchParallelRoute(int switchRoadType, LocInfoBean locInfoBean, BigInteger roadID, short flag, short hwFlag);

    /**
     * 获取请求id
     */
    long getRequestRouteId();

    /**
     * 发送L2++数据
     * @param routeCurrentPathParam L2
     */
    void sendL2Data(RouteCurrentPathParam routeCurrentPathParam);

    void requestRouteDetails(int index);

    void requestRouteRestArea(int index);
}
