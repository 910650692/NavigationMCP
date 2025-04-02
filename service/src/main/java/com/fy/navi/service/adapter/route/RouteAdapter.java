package com.fy.navi.service.adapter.route;

import com.android.utils.ConvertUtils;
import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @author lvww
 * @version  \$Revision.1.0\$
 * description TODO
 * date 2024/12/5
 */
final public class RouteAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(RouteAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "RouteAdapterImpl";

    public static final String REGISTRE_FROM_ROUTE = "register_from_route";

    private IRouteApi mRouteApi;
    private Map<MapType, List<RouteParam>> mParamMap;
    private Map<MapType, RouteCurrentPathParam> mRouteCurrentPathParamMap;

    private RouteAdapter() {
        mParamMap = new HashMap<>();
        mRouteCurrentPathParamMap = new HashMap<>();
        mRouteApi = (IRouteApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public Map<MapType, List<RouteParam>> getParamMap() {
        return mParamMap;
    }

    public Map<MapType, RouteCurrentPathParam> getRouteCurrentPathParamMap() {
        return mRouteCurrentPathParamMap;
    }

    /**
     * 初始化算路服务
     */
    public void initRouteService() {
        mRouteApi.initRouteService();
    }

    /**
     * 注册算路回调监听
     * @param key key
     * @param routeResultObserver 回调监听对象
     */
    public void registerRouteObserver(final String key, final RouteResultObserver routeResultObserver) {
        mRouteApi.registerRouteObserver(key, routeResultObserver);
    }

    /**
     * 反初始化路线服务
     */
    public void unInitRouteService() {
        mRouteApi.unInitRouteService();
    }

    public static RouteAdapter getInstance() {
        return Helper.RA;
    }

    /**
     * 设置路线偏好
     * @param routePreferenceID 偏好Id
     */
    public void setRoutePreference(final RoutePreferenceID routePreferenceID) {
        mRouteApi.setRoutePreference(routePreferenceID);
    }

    /**
     * 基本算路请求
     *
     * @param param 算路请求参数
     * @param paramList 点信息
     * @return 返回请求taskId
     */
    public long requestRoute(final RouteRequestParam param, final List<RouteParam> paramList) {
        return mRouteApi.requestRoute(param, paramList);
    }

    /**
     * 保存所有点信息
     *
     * @param mapTypeId 屏幕Id
     * @param paramList 点信息
     */
    public void saveAllPoiParamList(final MapType mapTypeId, final List<RouteParam> paramList) {
        mParamMap.put(mapTypeId, paramList);
    }

    /**
     * 获取所有点信息
     *
     * @param mapTypeId 屏幕Id
     * @return 返回所有点信息
     */
    public List<RouteParam> getAllPoiParamList(final MapType mapTypeId) {
        final List<RouteParam> routeParams = new ArrayList<>();
        if (!ConvertUtils.isEmpty(mParamMap.get(mapTypeId))) {
            routeParams.addAll(mParamMap.get(mapTypeId));
        }
        return routeParams;
    }

    /**
     * 请求路线上的天气
     *
     * @param routeLineLayerParam 线路数据
     * @param index 路线ID
     * @return 返回请求的taskId
     */
    public long requestRouteWeather(final RouteLineLayerParam routeLineLayerParam, final int index) {
        return mRouteApi.requestRouteWeather(routeLineLayerParam, index);
    }

    /**
     * 请求备选充电站
     *
     * @param pathInfo 算路信息
     * @param poiId 推荐充电站
     * @return 返回请求的taskId
     */
    public long requestRouteAlternativeChargeStation(final Object pathInfo, final String poiId) {
        return mRouteApi.requestRouteAlternativeChargeStation(pathInfo, poiId);
    }

    /**
     * 取消算路
     *
     * @param requestId 请求Id
     * @return 返回是否成功
     */
    public boolean cancelRoute(final long requestId) {
        return mRouteApi.cancelRoute(requestId);
    }

    /**
     * 设置避开道路
     * @param routeAvoidInfo 避开参数
     */
    public void setAvoidRoad(final RouteAvoidInfo routeAvoidInfo) {
        mRouteApi.setAvoidRoad(routeAvoidInfo);
    }

    /**
     * 设置请求控制
     * @param id 偏好
     * @param num 车牌
     * @param restriction 是否避开实现
     * @param routePlan 引导是否激活
     */
    public void setRequestControl(final RoutePreferenceID id, final String num, final boolean restriction, final boolean routePlan) {
        mRouteApi.setRequestControl(id, num, restriction, routePlan);
    }

    /**
     * 设置当前线路
     * @param routeCurrentPathParam 当前线路信息
     */
    public void setCurrentPath(final RouteCurrentPathParam routeCurrentPathParam) {
        mRouteCurrentPathParamMap.put(routeCurrentPathParam.getMMapTypeId(), routeCurrentPathParam);
    }

    /**
     * 获取当前线路
     * @param mapTypeId 屏幕Id
     * @return 当前线路信息
     */
    public RouteCurrentPathParam getCurrentPath(final MapType mapTypeId) {
        if (!ConvertUtils.isEmpty(mRouteCurrentPathParamMap.get(mapTypeId))) {
            return mRouteCurrentPathParamMap.get(mapTypeId);
        }
        return null;
    }

    /**
     * 设置终点
     * @param poiInfoEntity 终点信息
     */
    public void sendEndEntity(final PoiInfoEntity poiInfoEntity) {
        mRouteApi.sendEndEntity(poiInfoEntity);
    }

    /**
     * 路线还原，手机算路
     * @param routeMsgPushInfo 算路还原参数
     * @param mapTypeId 屏幕Id
     */
    public void requestRouteRestoration(final RouteMsgPushInfo routeMsgPushInfo, final MapType mapTypeId) {
        mRouteApi.requestRouteRestoration(routeMsgPushInfo,mapTypeId);
    }
    /**
     * 取消算路
     * @param requestId 算路请求ID
     */
    public void abortRequest(final long requestId) {
        mRouteApi.abortRequest(requestId);
    }

    private static final class Helper {
        private static final RouteAdapter RA = new RouteAdapter();
    }
}
