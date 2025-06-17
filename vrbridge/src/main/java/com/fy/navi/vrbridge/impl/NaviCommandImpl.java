package com.fy.navi.vrbridge.impl;

import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.baidu.oneos.protocol.bean.ArrivalBean;
import com.baidu.oneos.protocol.bean.CallResponse;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.listener.NaviCommandListener;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.vrbridge.IVrBridgeConstant;
import com.fy.navi.vrbridge.MapStateManager;
import com.fy.navi.vrbridge.VrBridgeManager;
import com.fy.navi.vrbridge.bean.SingleCommandInfo;

public class NaviCommandImpl implements NaviCommandListener {

    public NaviCommandImpl() {

    }

    /**
     * 导航搜索:
     *     单目的地；
     *     多目的地（最多4个）；
     *     带路线偏好；
     *     带搜索条件（最多3个）
     * @param sessionId  用来确保本轮数据的一致性
     * @param dest
     *          private String dest : 导航目的地，有以下2中类型
     *                                1. 地址：有目的地，比如 世界之窗，深圳北站
     *                                2. 常量：HOME：家；COMPANY：公司
     *          private String destType : 目的地类型
     *          private Map(String, String) conditions : 目的地的检索条件
     *          private String routeType : 路线偏好
     *          private SparseArray(string) passbyPoi : 多目的地导航
     *          private String appName : 用指定地图发起导航
     *                                   BAIDU_MAP：百度地图；
     *                                   A_MAP：高德地图；
     *          详情见接口文档
     * @param poiCallback 异步接收检索的结果
     * @return CallResponse
     */
    @Override
    public CallResponse onRouteNavi(final String sessionId, final ArrivalBean dest, final PoiCallback poiCallback) {
        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onRouteNavi: sessionId = ", sessionId, ", dest = ", dest);
        }
        final boolean saveCommand = MapStateManager.getInstance().openMapWhenBackground();
        if (saveCommand) {
            VrBridgeManager.getInstance().saveNaviCommand(sessionId, dest, poiCallback);
            return CallResponse.createSuccessResponse();
        } else {
            return VoiceSearchManager.getInstance().handleCommonSearch(sessionId, dest, poiCallback);
        }
    }

    /**
     * 沿途的; 沿途的卫生间
     * @param sessionId  用来确保本轮数据的一致性
     * @param passBy 1. null 比如沿途的，需要进入多轮
     *               2. 途径点
     * @param poiType 参数说明同ArrivalBean.destType
     * @param poiCallback 异步接收检索结果
     * @return CallResponse
     */
    @Override
    public CallResponse onPassbySearch(final String sessionId, final String passBy, final String poiType, final PoiCallback poiCallback) {
        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onPassBySearch: sessionId = ", sessionId, ", passBy = ", passBy, ", poiType = ", poiType);
        }
        MapStateManager.getInstance().openMapWhenBackground();

        if (TextUtils.isEmpty(sessionId) || TextUtils.isEmpty(passBy)) {
            Logger.e(IVrBridgeConstant.TAG, "session or passBy is empty");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.PASS_BY_PARAM_EMPTY);
        }
        if (!MapStateManager.getInstance().isNaviStatus()) {
            //非导航态不支持沿途搜
            Logger.w(IVrBridgeConstant.TAG, "alongSearch in no navigation");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NAVI_BEFORE_PASSBY_ROUTE);
        }
        final RouteCurrentPathParam pathParam = RoutePackage.getInstance().getCurrentPathInfo(MapType.MAIN_SCREEN_MAIN_MAP);
        if (null != pathParam && !pathParam.isMIsOnlineRoute()) {
            //离线算路不支持沿途搜
            Logger.w(IVrBridgeConstant.TAG, "alongSearch in offline road");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.OFFLINE_NOT_SUPPORT);
        }

        VoiceSearchManager.getInstance().handlePassBy(sessionId, passBy, poiType, poiCallback);
        return CallResponse.createSuccessResponse();
    }

    @Override
    public CallResponse onPassbyAdd(final String s, final String s1, final String s2, final PoiCallback poiCallback) {
        return CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_THIS_FUNCTION);
    }

}
