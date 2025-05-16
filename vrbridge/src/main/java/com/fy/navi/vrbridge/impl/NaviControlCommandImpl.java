package com.fy.navi.vrbridge.impl;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.baidu.oneos.protocol.bean.CallResponse;
import com.baidu.oneos.protocol.bean.TrafficAskBean;
import com.baidu.oneos.protocol.bean.param.NaviControlParam;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.callback.RespCallback;
import com.baidu.oneos.protocol.listener.NaviControlCommandListener;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviExchangeEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.vrbridge.IVrBridgeConstant;
import com.fy.navi.vrbridge.MapStateManager;
import com.fy.navi.vrbridge.VoiceConvertUtil;
import com.fy.navi.vrbridge.bean.MapState;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class NaviControlCommandImpl implements NaviControlCommandListener {

    /*---------------------------------------保存个性化道路上一轮找到的路线信息 start-----------------*/
    private NaviExchangeEntity.NewRoute mNewRoute = null; //引导态返回的路线信息
    /*---------------------------------------保存个性化道路上一轮找到的路线信息 end--------------------*/


    /**
     * 放大/缩小地图
     *
     * @param zoomIn       true:放大; false:缩小
     * @param size         Integer.MAX_VALUE: 放大到最大；Integer.MIN_VALUE: 缩小到最小；1: 默认放大\缩小
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onMapSizeAdjust(final boolean zoomIn, final int size, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onMapSizeAdjust: zoomIn = " + zoomIn + ", size = " + size);

        openMapWhenBackground();

        final CallResponse callResponse;
        final float curSize = MapPackage.getInstance().getZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        final float levelSize;
        switch (size) {
            case Integer.MAX_VALUE:
                levelSize = AutoMapConstant.MAP_ZOOM_LEVEL_MAX;
                break;
            case Integer.MIN_VALUE:
                levelSize = AutoMapConstant.MAP_ZOOM_LEVEL_MIN;
                break;
            case 1:
                levelSize = AutoMapConstant.MAP_ZOOM_LEVEL_CHANGE_FLAG;
                break;
            default:
                Logger.d(IVrBridgeConstant.TAG, "Error zoom action !");
                return CallResponse.createFailResponse("不支持的的缩放指令");
        }

        if (zoomIn) {
            if (curSize == AutoMapConstant.MAP_ZOOM_LEVEL_MAX) {
                callResponse = CallResponse.createSuccessResponse("当前已为最大地图");
            } else {
                if (levelSize == AutoMapConstant.MAP_ZOOM_LEVEL_MAX) {
                    //直接放到最大
                    Logger.d(IVrBridgeConstant.TAG, "Set map level max");
                    MapPackage.getInstance().setZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP, levelSize);
                    callResponse = CallResponse.createSuccessResponse("已放大地图到最大");
                } else {
                    //默认flag提升
                    Logger.d(IVrBridgeConstant.TAG, "Amplify map level");
                    MapPackage.getInstance().amplifyLevel(MapType.MAIN_SCREEN_MAIN_MAP);
                    callResponse = CallResponse.createSuccessResponse("地图已放大");
                }
            }
        } else {
            if (curSize == AutoMapConstant.MAP_ZOOM_LEVEL_MIN) {
                callResponse = CallResponse.createSuccessResponse("当前已为最小地图");
            } else {
                if (levelSize == AutoMapConstant.MAP_ZOOM_LEVEL_MIN) {
                    Logger.d(IVrBridgeConstant.TAG, "Set map level min");
                    MapPackage.getInstance().setZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP, levelSize);
                    callResponse = CallResponse.createSuccessResponse("已缩小地图到最小");
                } else {
                    Logger.d(IVrBridgeConstant.TAG, "Reduce map level");
                    MapPackage.getInstance().reduceLevel(MapType.MAIN_SCREEN_MAIN_MAP);
                    callResponse = CallResponse.createSuccessResponse("地图已缩小");
                }
            }
        }
        Logger.d(IVrBridgeConstant.TAG, "map state : " + MapState.getInstance().getCurrZoomLevel());
        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, respCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 打开/关闭路线全览
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onRouteOverviewToggle(final boolean open, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onRouteOverviewToggle: open = " + open);
        final boolean inNavigation = MapStateManager.getInstance().isNaviStatus();
        final boolean preview = NaviPackage.getInstance().getPreviewStatus();
        final CallResponse callResponse;
        if (open) {
            //查看全程
            if (inNavigation) {
                if (preview) {
                    callResponse = CallResponse.createNotSupportResponse("当前已展示路线全览");
                } else {
                    NaviPackage.getInstance().voiceRouteOverview(MapType.MAIN_SCREEN_MAIN_MAP, true);
                    callResponse = CallResponse.createSuccessResponse("已展示路线全览");
                }
            } else {
                callResponse = CallResponse.createNotSupportResponse("当前不在导航状态，无法查看路线全览哦");
            }
        } else {
            //退出全程
            if (inNavigation) {
                if (preview) {
                    NaviPackage.getInstance().voiceRouteOverview(MapType.MAIN_SCREEN_MAIN_MAP, false);
                    callResponse = CallResponse.createSuccessResponse("已退出路线全览");
                } else {
                    callResponse = CallResponse.createNotSupportResponse("当前未展示路线全览");
                }
            } else {
                openMapWhenBackground();
                callResponse = CallResponse.createNotSupportResponse("当前未展示路线全览");
            }
        }

        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, respCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 打开/关闭经典导航
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     * @deprecated
     */
    @Override
    @Deprecated
    public CallResponse onClassicNaviToggle(final boolean open, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onClassicNaviToggle: open = " + open);
        return CallResponse.createFailResponse("暂不支持切换经典导航");
    }

    /**
     * 打开/关闭轻导航
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     * @deprecated
     */
    @Override
    @Deprecated
    public CallResponse onFamiliarNaviToggle(final boolean open, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onFamiliarNaviToggle: open = " + open);
        return responseNotSupport();
    }

    /**
     * 不支持的功能统一回复.
     *
     * @return CallResponse.
     */
    private CallResponse responseNotSupport() {
        return CallResponse.createFailResponse("不支持此功能");
    }

    /**
     * 打开/关闭AR导航
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     * @deprecated
     */
    @Override
    @Deprecated
    public CallResponse onArNaviToggle(final boolean open, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onArNaviToggle: open = " + open);
        return responseNotSupport();
    }

    /**
     * 切换导航模式
     *
     * @param respCallback respCallback
     * @return CallResponse
     * @deprecated
     */
    @Override
    @Deprecated
    public CallResponse onNaviModeSwitch(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onFamiliarNaviToggle:");
        return responseNotSupport();
    }

    /**
     * 开始/结束/继续导航
     *
     * @param action       执行动作：
     *                     START_NAVIGATION:开始导航;
     *                     END_NAVIGATION:结束导航;
     *                     CONTINUE_NAVIGATION:继续导航;
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviActionChange(final String action, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onNaviActionChange: action = " + action);

        final CallResponse callResponse;
        switch (action) {
            case IVrBridgeConstant.NavigationOperateType.START:
                if (MapStateManager.getInstance().isNaviStatus()) {
                    Logger.d(IVrBridgeConstant.TAG, "Already in navigation state");
                    openMapWhenBackground();
                    callResponse = CallResponse.createSuccessResponse("当前已在导航中");
                } else if (MapStateManager.getInstance().inSelectRoute()) {
                    Logger.d(IVrBridgeConstant.TAG, "Selecting route page, start navigation");
                    callResponse = CallResponse.createSuccessResponse("开始出发");
                    final Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.START_NAVIGATION);
                    MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                } else {
                    Logger.d(IVrBridgeConstant.TAG, "Haven't select route, keep listen");
                    callResponse = CallResponse.createSuccessResponse("请问你要去哪里呢");
                }
                break;
            case IVrBridgeConstant.NavigationOperateType.STOP:
                if (MapStateManager.getInstance().isNaviStatus()) {
                    Logger.d(IVrBridgeConstant.TAG, "Stop navigation successfully");
                    stopNavigation();
                    callResponse = CallResponse.createSuccessResponse("导航结束");
                } else {
                    Logger.d(IVrBridgeConstant.TAG, "Already end navigation");
                    callResponse = CallResponse.createSuccessResponse("当前不在导航状态");
                }
                break;
            case IVrBridgeConstant.NavigationOperateType.CONTINUE:
                final int sceneState = NaviPackage.getInstance().getCurrentImmersiveStatus();
                Logger.d(IVrBridgeConstant.TAG, "Continue navigation sceneState: " + sceneState);
                if (sceneState == 0) {
                    //触碰态
                    NaviPackage.getInstance().voiceContinueNavigation(MapType.MAIN_SCREEN_MAIN_MAP);
                    callResponse = CallResponse.createSuccessResponse("已恢复导航");
                } else {
                    //非触碰态
                    callResponse = CallResponse.createSuccessResponse("好的");
                }
                break;
            default:
                Logger.d(IVrBridgeConstant.TAG, "Error navi action");
                callResponse = CallResponse.createFailResponse("不支持的的导航指令");
                break;
        }

        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, respCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 停止导航，清空底图路线、设置默认状态.
     */
    private void stopNavigation() {
        RoutePackage.getInstance().clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
        NaviPackage.getInstance().stopNavigation();
        NaviPackage.getInstance().setPreviewStatus(false);
    }

    /**
     * 打开/关闭巡航模式
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onEDogModeToggle(final boolean open, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onEDogModeToggle: open = " + open);
        openMapWhenBackground();

        final CallResponse response;
        if (open) {
            response = CallResponse.createNotSupportResponse("不支持直接打开巡航");
        } else {
            final boolean inCruise = MapStateManager.getInstance().inCruiseStatus();
            if (inCruise) {
                if (CruisePackage.getInstance().stopCruise()) {
                    response = CallResponse.createSuccessResponse("巡航已关闭");
                } else {
                    response = CallResponse.createFailResponse("巡航关闭失败");
                }
            } else {
                response = CallResponse.createNotSupportResponse("当前不处于巡航态");
            }
        }

        response.setNeedPlayMessage(true);
        respTts(response, respCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 打开/关闭路况
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     * @deprecated
     */
    @Override
    @Deprecated
    public CallResponse onTrafficModeToggle(final boolean open, final RespCallback respCallback) {
        final boolean curTrafficMode = SettingPackage.getInstance().getConfigKeyRoadEvent();
        Logger.d(IVrBridgeConstant.TAG, "onTrafficModeToggle open: " + open + ", curStatus: " + curTrafficMode);
        openMapWhenBackground();

        final StringBuilder builder = new StringBuilder();
        if (open == curTrafficMode) {
            builder.append("当前").append(IVrBridgeConstant.ROAD_CONDITION).append("已").append(open ? "打开" : "关闭");
        } else {
            MapPackage.getInstance().setTrafficStates(MapType.MAIN_SCREEN_MAIN_MAP, open);
            builder.append("已").append(open ? "打开" : "关闭").append(IVrBridgeConstant.ROAD_CONDITION);
        }
        final CallResponse response = CallResponse.createSuccessResponse(builder.toString());
        response.setNeedPlayMessage(true);
        respTts(response, respCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 走某道路
     *
     * @param road         String，选定的道路名称
     * @param respCallback RespCallback，语音结果回调.
     * @return CallResponse.
     */
    @Override
    public CallResponse onRoadAssigned(final String road, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onRoadAssigned: road = " + road);
        if (TextUtils.isEmpty(road)) {
            Logger.w(IVrBridgeConstant.TAG, "specialRoad empty");
            return CallResponse.createFailResponse("不支持的参数");
        }

        final String curNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        final int viaCount = RoutePackage.getInstance().getViaPointsCount(MapType.MAIN_SCREEN_MAIN_MAP);
        switch (curNaviStatus) {
            case NaviStatus.NaviStatusType.SELECT_ROUTE:
                if (viaCount > 0) {
                    //添加途径点后不支持指定路线
                    return notSupportSpecialRoad();
                } else {
                    //获取路线结果，匹配当前选择路线与剩余路线
                    chooseRoadWhenSelect(road, respCallback);
                }
                break;
            case NaviStatus.NaviStatusType.NAVING:
            case NaviStatus.NaviStatusType.LIGHT_NAVING:
                if (viaCount > 0) {
                    return notSupportSpecialRoad();
                } else {
                    chooseRoadWhenNavigation(road, respCallback);
                }
                break;
            default:
                return responseStartNaviFirst();
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 添加途径点后不支持个性化道路回复.
     *
     * @return CallResponse 语音执行结果.
     */
    private CallResponse notSupportSpecialRoad() {
        return CallResponse.createFailResponse("添加途经点后不支持该功能");
    }

    /**
     * 不在选路态/引导态请先发起导航提示.
     *
     * @return CallResponse 提示信息回复.
     */
    private CallResponse responseStartNaviFirst() {
        return CallResponse.createFailResponse("请先发起导航");
    }

    /**
     * 选择某条路.
     *
     * @param roadName     选定道路名称.
     * @param respCallback RespCallback,语音异步回调.
     */
    private void chooseRoadWhenSelect(final String roadName, final RespCallback respCallback) {
        if (null == roadName || roadName.isEmpty()) {
            Logger.d(IVrBridgeConstant.TAG, "chooseRoad name are empty");
            return;
        }

        boolean hasMatched = false;
        int chooseIndex = -1;
        final List<RouteLineInfo> routeLineList = MapStateManager.getInstance().getRouteList();
        try {
            final int size = routeLineList.size();
            outLoop:
            for (int i = 0; i < size; i++) {
                final RouteLineInfo lineInfo = routeLineList.get(i);
                if (null == lineInfo || null == lineInfo.getMRouteLineSegmentInfos()
                        || lineInfo.getMRouteLineSegmentInfos().isEmpty()) {
                    continue;
                }

                final List<RouteLineSegmentInfo> segmentList = lineInfo.getMRouteLineSegmentInfos();
                for (RouteLineSegmentInfo segmentInfo : segmentList) {
                    if (segmentInfo.getMLoadName().contains(roadName)) {
                        hasMatched = true;
                        chooseIndex = i;
                        break outLoop;
                    }
                }
            }
        } catch (NullPointerException exception) {
            Logger.e(IVrBridgeConstant.TAG, "match road error: " + exception.getMessage());
        } catch (IndexOutOfBoundsException outOfBoundsException) {
            Logger.e(IVrBridgeConstant.TAG, "match road index error: " + outOfBoundsException.getMessage());
        }
        Logger.d(IVrBridgeConstant.TAG, "assignRoad chooseRoute: " + chooseIndex);

        if (hasMatched) {
            try {
                responseSpecialRoad(true, roadName, routeLineList.get(chooseIndex), respCallback);
                final Map<MapType, Integer> indexMap = RoutePackage.getInstance().getSelectRouteIndex();
                final Integer curSelectValue = indexMap.getOrDefault(MapType.MAIN_SCREEN_MAIN_MAP, -1);
                final int mainIndex = null != curSelectValue ? curSelectValue : 0;
                Logger.d(IVrBridgeConstant.TAG, "assignRoad curSelect: " + mainIndex);
                if (mainIndex == chooseIndex) {
                    //与当前已选择路线一致，直接发起引导
                    final Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.START_NAVIGATION);
                    MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                } else {
                    final Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.SELECT_ROUTE);
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.ROUTE_INDEX, chooseIndex);
                    MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                }
            } catch (NullPointerException nullException) {
                Logger.w(IVrBridgeConstant.TAG, "match road, getSelect index empty: " + nullException.getMessage());
            } catch (IndexOutOfBoundsException iobException) {
                Logger.w(IVrBridgeConstant.TAG, "match road, getSelect index outOfBound: " + iobException.getMessage());
            }
        } else {
            final CallResponse response = CallResponse.createFailResponse("没有找到走" + roadName + "的合理路线");
            response.setNeedPlayMessage(true);
            respTts(response, respCallback);
        }
    }

    /**
     * 引导态指定走某条道路.
     *
     * @param road         String，指定道路名称.
     * @param respCallback 执行结果回调.
     */
    private void chooseRoadWhenNavigation(final String road, final RespCallback respCallback) {
        final NaviExchangeEntity naviExchangeEntity = NaviPackage.getInstance()
                .getExchangeResult(road, 1, MapType.MAIN_SCREEN_MAIN_MAP);
        final int changeType = naviExchangeEntity.getExchangeType();
        final StringBuilder builder = new StringBuilder();
        final CallResponse response;
        switch (changeType) {
            case NaviExchangeEntity.ExchangeType.ROAD_HAS_PASSED:
                mNewRoute = null;
                response = CallResponse.createNotSupportResponse("当前线路已通过此道路");
                break;
            case NaviExchangeEntity.ExchangeType.ROAD_IN_CURRENT_ROUTE:
                mNewRoute = null;
                builder.append("当前路线途径").append(road);
                response = CallResponse.createSuccessResponse(builder.toString());
                break;
            case NaviExchangeEntity.ExchangeType.NO_ROUTE:
                mNewRoute = null;
                builder.append("没有找到走").append(road).append(IVrBridgeConstant.ROUTE_HINT);
                response = CallResponse.createFailResponse(builder.toString());
                break;
            case NaviExchangeEntity.ExchangeType.NORMAL_EXCHANGE:
                mNewRoute = naviExchangeEntity.getNewRoute();
                builder.append("找到走").append(road).append(IVrBridgeConstant.ROUTE_HINT);
                int compareTime = mNewRoute.getComePareTime();
                if (compareTime > 0) {
                    builder.append("，慢");
                } else {
                    builder.append("，快");
                    compareTime = Math.abs(compareTime);
                }
                builder.append(VoiceConvertUtil.formatTime(compareTime)).append("，确定切换吗");
                response = CallResponse.createSuccessResponse(builder.toString());
                break;
            default:
                mNewRoute = null;
                Logger.w(IVrBridgeConstant.TAG, "unHandle naviExchangeType: " + changeType);
                response = CallResponse.createFailResponse("没有找到走" + road + "的信息");
                break;
        }

        response.setNeedPlayMessage(true);
        respTts(response, respCallback);
    }

    /**
     * 统一回复语音个性化道路.
     *
     * @param assign        指令类型，true-走某条路  false-不走某条路.
     * @param road          String，指定道路名称.
     * @param routeLineInfo RouteLineInfo 匹配路线信息.
     * @param respCallback  RespCallback,语音结果回调.
     */
    private void responseSpecialRoad(final boolean assign, final String road,
                                     final RouteLineInfo routeLineInfo, final RespCallback respCallback) {
        if (null == routeLineInfo || TextUtils.isEmpty(routeLineInfo.getMLength())) {
            return;
        }
        final StringBuilder builder = new StringBuilder("找到");
        if (!assign) {
            builder.append("不");
        }
        builder.append("走").append(road).append("的路线，").append("全程").append(routeLineInfo.getMLength())
                .append("，预计").append(routeLineInfo.getMTravelTime());
        final CallResponse response = CallResponse.createSuccessResponse(builder.toString());
        response.setNeedPlayMessage(true);
        respTts(response, respCallback);
    }

    /**
     * 不走某道路
     *
     * @param road         String, 避开道路名称
     * @param respCallback RespCallback.
     * @return CallResponse.
     */
    @Override
    public CallResponse onRoadNonAssigned(final String road, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onRoadNonAssigned: road = " + road);
        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        final int viaCount = RoutePackage.getInstance().getViaPointsCount(MapType.MAIN_SCREEN_MAIN_MAP);
        switch (curStatus) {
            case NaviStatus.NaviStatusType.SELECT_ROUTE:
                if (viaCount > 0) {
                    //添加途径点后不支持指定路线
                    return notSupportSpecialRoad();
                } else {
                    avoidRoadWhenSelect(road, respCallback);
                }
                break;
            case NaviStatus.NaviStatusType.NAVING:
            case NaviStatus.NaviStatusType.LIGHT_NAVING:
                if (viaCount > 0) {
                    return notSupportSpecialRoad();
                } else {
                    avoidRoadWhenNavigation(road, respCallback);
                }
                break;
            default:
                return responseStartNaviFirst();
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 获取当前选路结果，匹配不走执行road的路线.
     *
     * @param road         String,指定道路名称
     * @param respCallback RespCallback，语音结果回调.
     */
    private void avoidRoadWhenSelect(final String road, final RespCallback respCallback) {
        if (null == road || road.isEmpty()) {
            Logger.d(IVrBridgeConstant.TAG, "avoidRoad name are empty");
            return;
        }

        final List<RouteLineInfo> routeLineList = MapStateManager.getInstance().getRouteList();
        try {
            final int size = routeLineList.size();
            Logger.d(IVrBridgeConstant.TAG, "不走xxx路，size: " + size);
            final List<Boolean> matchedStatusList = new ArrayList<>();
            for (int i = 0; i < size; i++) {
                boolean matched = false;
                final RouteLineInfo lineInfo = routeLineList.get(i);
                final List<RouteLineSegmentInfo> segmentList = lineInfo.getMRouteLineSegmentInfos();
                for (RouteLineSegmentInfo segmentInfo : segmentList) {
                    if (segmentInfo.getMLoadName().contains(road)) {
                        matched = true;
                    }
                }
                matchedStatusList.add(matched);
            }

            final Map<MapType, Integer> indexMap = RoutePackage.getInstance().getSelectRouteIndex();
            final Integer curSelectValue = indexMap.getOrDefault(MapType.MAIN_SCREEN_MAIN_MAP, -1);
            final int curIndex = null != curSelectValue ? curSelectValue : 0;
            Logger.d(IVrBridgeConstant.TAG, "afterProcess matchStatusSize: " + matchedStatusList.size()
                    + ", currentSelectIndex: " + curIndex);
            int selectIndex = -1;
            if (matchedStatusList.get(curIndex)) {
                final int matchedStatusSize = matchedStatusList.size();
                for (int i = 0; i < matchedStatusSize; i++) {
                    if (i == curIndex) {
                        continue;
                    }
                    if (!matchedStatusList.get(i)) {
                        selectIndex = i;
                        break;
                    }
                }
                if (selectIndex >= 0) {
                    responseSpecialRoad(false, road, routeLineList.get(selectIndex), respCallback);
                    //匹配到非当前所选路线，选路界面当前所选路线
                    final Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.SELECT_ROUTE);
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.ROUTE_INDEX, selectIndex);
                    MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                } else {
                    final CallResponse response = CallResponse.createFailResponse("没有找到不走" + road + "的合理路线");
                    response.setNeedPlayMessage(true);
                    respTts(response, respCallback);
                }
            } else {
                responseSpecialRoad(false, road, routeLineList.get(curIndex), respCallback);
                //当前道路可以避开指定道路，直接发起导航
                final Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.START_NAVIGATION);
                MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            }
        } catch (NullPointerException exception) {
            Logger.e(IVrBridgeConstant.TAG, "avoid road error: " + exception.getMessage());
        } catch (IndexOutOfBoundsException outOfBoundsException) {
            Logger.e(IVrBridgeConstant.TAG, "avoid road index error: " + outOfBoundsException.getMessage());
        }
    }

    /**
     * 引导态避开指定道路.
     *
     * @param road         String，指定道路名称.
     * @param respCallback RespCallback，执行结果回调.
     */
    private void avoidRoadWhenNavigation(final String road, final RespCallback respCallback) {
        final NaviExchangeEntity naviExchangeEntity = NaviPackage.getInstance().getExchangeResult(road, 0, MapType.MAIN_SCREEN_MAIN_MAP);
        final int changeType = naviExchangeEntity.getExchangeType();
        final StringBuilder builder = new StringBuilder();
        final CallResponse response;
        switch (changeType) {
            case NaviExchangeEntity.ExchangeType.ROAD_HAS_PASSED:
                mNewRoute = null;
                response = CallResponse.createNotSupportResponse("当前线路已通过此道路");
                break;
            case NaviExchangeEntity.ExchangeType.ROAD_NOT_IN_CURRENT_ROUTE:
                mNewRoute = null;
                builder.append("您当前行驶的路线已避开").append(road);
                response = CallResponse.createSuccessResponse(builder.toString());
                break;
            case NaviExchangeEntity.ExchangeType.NO_ROUTE:
                mNewRoute = null;
                builder.append("没有找到不走").append(road).append("的路线");
                response = CallResponse.createFailResponse(builder.toString());
                break;
            case NaviExchangeEntity.ExchangeType.NORMAL_EXCHANGE:
                mNewRoute = naviExchangeEntity.getNewRoute();
                builder.append("找到不走").append(road).append("的路线，");
                int compareTime = mNewRoute.getComePareTime();
                if (compareTime > 0) {
                    builder.append("慢");
                } else {
                    builder.append("快");
                    compareTime = Math.abs(compareTime);
                }
                builder.append(VoiceConvertUtil.formatTime(compareTime)).append("，确定切换吗");
                response = CallResponse.createSuccessResponse(builder.toString());
                break;
            default:
                Logger.w(IVrBridgeConstant.TAG, "unHandle avoidRoad type: " + changeType);
                response = CallResponse.createFailResponse("没有找到不走" + road + "的信息");
                break;
        }

        response.setNeedPlayMessage(true);
        respTts(response, respCallback);
    }

    /**
     * 刷新路线
     *
     * @param respCallback respCallback
     * @return CallResponse
     * @deprecated
     */
    @Override
    @Deprecated
    public CallResponse onRouteRefresh(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onRouteRefresh:");
        if (MapStateManager.getInstance().isNaviStatus()) {
            final RouteRequestParam param = new RouteRequestParam();
            param.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
            param.setMRouteWay(RouteWayID.ROUTE_WAY_REFRESH);
            param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH);
            RoutePackage.getInstance().requestRoute(param);
            final CallResponse response = CallResponse.createSuccessResponse("已为你更新路线");
            response.setNeedPlayMessage(true);
            respTts(response, respCallback);
        } else {
            return CallResponse.createFailResponse("请先发起导航");
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 切换路线
     * 智能推荐/不走高速/少收费/躲避拥堵/时间优先/高速优先的顺序轮换
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onRouteSwitch(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onRouteSwitch:");
        final RoutePreferenceID curRouteId = SettingPackage.getInstance().getRoutePreference();
        final RoutePreferenceID targetId;
        switch (curRouteId) {
            case PREFERENCE_RECOMMEND:
                targetId = RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
                break;
            case PREFERENCE_NOTHIGHWAY:
                targetId = RoutePreferenceID.PREFERENCE_LESSCHARGE;
                break;
            case PREFERENCE_LESSCHARGE:
                targetId = RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
                break;
            case PREFERENCE_AVOIDCONGESTION:
                targetId = RoutePreferenceID.PREFERENCE_FASTESTSPEED;
                break;
            case PREFERENCE_FASTESTSPEED:
                targetId = RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
                break;
            case PREFERENCE_FIRSTHIGHWAY:
                targetId = RoutePreferenceID.PREFERENCE_FIRSTMAINROAD;
                break;
            case PREFERENCE_FIRSTMAINROAD:
            default:
                targetId = RoutePreferenceID.PREFERENCE_RECOMMEND;
                break;
        }

        SettingPackage.getInstance().setRoutePreferenceByVoice(targetId);
        final CallResponse response = CallResponse.createSuccessResponse("已切换路线偏好");
        response.setNeedPlayMessage(true);
        respTts(response, respCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 切换到XXX的路线
     *
     * @param type         CHOOSE_RECOMMEND_ROUTE:智能推荐 1
     *                     CHOOSE_NOHIGHWAY_ROUTE:不走高速 4;
     *                     CHOOSE_CHEAPER_ROUTE:少收费 8;
     *                     CHOOSE_AVOID_JAM:躲避拥堵 16;
     *                     CHOOSE_FASTER_ROUTE:时间短 256;
     *                     CHOOSE_HIGHWAY_ROUTE:高速优先 512;
     *                     CHOOSE_AUTOP_ROUTE:智驾/领航优先
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onRouteChoose(final String type, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onRoadChoose: road = " + type);
        final CallResponse callResponse;

        //用于存入settingPackage
        final RoutePreferenceID routeId;
        //用于返回tts
        final String tts;
        switch (type) {
            case "CHOOSE_RECOMMEND_ROUTE":
                routeId = RoutePreferenceID.PREFERENCE_RECOMMEND;
                tts = "智能推荐";
                break;
            case "CHOOSE_NOHIGHWAY_ROUTE":
                routeId = RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
                tts = "不走高速";
                break;
            case "CHOOSE_CHEAPER_ROUTE":
                routeId = RoutePreferenceID.PREFERENCE_LESSCHARGE;
                tts = "少收费";
                break;
            case "CHOOSE_AVOID_JAM":
                routeId = RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
                tts = "躲避拥堵";
                break;
            case "CHOOSE_FASTER_ROUTE":
                routeId = RoutePreferenceID.PREFERENCE_FASTESTSPEED;
                tts = "时间优先";
                break;
            case "CHOOSE_HIGHWAY_ROUTE":
                routeId = RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
                tts = "高速优先";
                break;
            case "CHOOSE_MAINROAD_ROUTE":
                routeId = RoutePreferenceID.PREFERENCE_FIRSTMAINROAD;
                tts = "大路优先";
                break;
            default:
                Logger.d(IVrBridgeConstant.TAG, "Go default case, no preference match currently, return !");
                return CallResponse.createFailResponse("不支持的偏好类型");
        }

        openMapWhenBackground();
        final RoutePreferenceID curPrefer = SettingPackage.getInstance().getRoutePreference();
        Logger.d(IVrBridgeConstant.TAG, "curRoutePrefer: " + curPrefer + ", target: " + routeId);
        if (routeId == curPrefer) {
            callResponse = CallResponse.createSuccessResponse("当前已是" + tts + "的路线");
        } else {
            SettingPackage.getInstance().setRoutePreferenceByVoice(routeId);
            callResponse = CallResponse.createSuccessResponse("已使用" + tts + "方案");
        }
        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 选择路线
     *
     * @param index        int，路线索引值.
     * @param respCallback RespCallback,执行结果回调.
     * @return CallResponse.
     */
    @Override
    public CallResponse onRouteChoose(final int index, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "Route Choose by index : road = " + index);
        if (MapStateManager.getInstance().inSelectRoute()) {
            final List<RouteLineInfo> routeLineInfoList = MapStateManager.getInstance().getRouteList();
            final int routeCount = null != routeLineInfoList ? routeLineInfoList.size() : 0;
            if (routeCount == 0) {
                return CallResponse.createFailResponse("当前路线规划结果为空");
            }
            final int routeIndex = index - 1;
            if (routeIndex < 0 || routeIndex >= routeCount) {
                return CallResponse.createFailResponse("不在选择范围内");
            }

            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.SELECT_ROUTE);
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.ROUTE_INDEX, routeIndex);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            final CallResponse response = CallResponse.createSuccessResponse("已选择第" + index + "条路线");
            response.setNeedPlayMessage(true);
            respCallback.onResponse(response);
            return CallResponse.createSuccessResponse();
        } else {
            return CallResponse.createFailResponse("不支持选则路线");
        }
    }

    /**
     * 设置常用地址——家和公司或收藏指定poi.
     *
     * @param sessionId   用来确保本轮数据的一致性
     * @param poiType     HOME：家；COMPANY：公司 (此两个是设置家公司地址) poiCollect:收藏指定poi
     * @param poi         地址，CURRENT_LOCATION：当前位置
     * @param poiCallback poiCallback
     * @return CallResponse 语音执行结果回复.
     */
    @Override
    public CallResponse onCommonPoiSet(final String sessionId, final String poiType, @NonNull final String poi, final PoiCallback poiCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onCommonPoiSet: sessionId = " + sessionId + ", poiType = " + poiType + ", poi = " + poi);
        switch (poiType) {
            case IVrBridgeConstant.DestType.POI_COLLECT:
                // 收藏指定poi
                return VoiceSearchManager.getInstance().searchPoiInfo(IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE, poi, poiCallback);
            case IVrBridgeConstant.DestType.HOME:
            case IVrBridgeConstant.DestType.COMPANY:
                //设置家/公司地址
                return VoiceSearchManager.getInstance().setHomeCompany(sessionId, poiType, poi, poiCallback);
            default:
                return CallResponse.createFailResponse("不支持的设置类型");
        }
    }

    /**
     * 位置查询
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onLocationAsk(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onLocationAsk");
        openMapWhenBackground();

        final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
        final double lon = null != locInfoBean ? locInfoBean.getLongitude() : 0.0;
        final double lat = null != locInfoBean ? locInfoBean.getLatitude() : 0.0;
        if (null == locInfoBean || Double.compare(lon, 0.0) <= 0 || Double.compare(lat, 0.0) <= 0) {
            Logger.e(IVrBridgeConstant.TAG, "curLocation is empty");
            return CallResponse.createFailResponse("不好意思，我定位不到你在哪里");
        } else {
            //搜索Poi详情
            Logger.d(IVrBridgeConstant.TAG, "ask location lon: " + lon + ", lat: " + lat);
            final GeoPoint geoPoint = new GeoPoint(lon, lat);
            VoiceSearchManager.getInstance().queryCurrentLocationDetail(IVrBridgeConstant.VoiceSearchType.SHOW_POI_DETAIL,
                    geoPoint, respCallback);
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 主辅路切换
     *
     * @param road         MAIN：主路；SIDE：辅路
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onRoadChange(final String road, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onRoadChange: road = " + road);
        CallResponse response = null;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final LocParallelInfoEntity parallelInfo = MapStateManager.getInstance().getParallelInfo();
            if (null != parallelInfo && parallelInfo.getStatus() == 1 && parallelInfo.getFlag() != 0) {
                final int flag = parallelInfo.getFlag();
                switch (road) {
                    case IVrBridgeConstant.ParallelOption.MAIN:
                        //切换到主路
                        if (flag == 2) {
                            response = CallResponse.createSuccessResponse("已切换到主路");
                            NaviPackage.getInstance().voiceChangeParallelRoad(MapType.MAIN_SCREEN_MAIN_MAP,
                                    IVrBridgeConstant.ParallelOption.MAIN);
                        } else {
                            response = CallResponse.createNotSupportResponse("当前已在主路");
                            openMapWhenBackground();
                        }
                        break;
                    case IVrBridgeConstant.ParallelOption.SIDE:
                        //切换到辅路
                        if (flag == 1) {
                            NaviPackage.getInstance().voiceChangeParallelRoad(MapType.MAIN_SCREEN_MAIN_MAP,
                                    IVrBridgeConstant.ParallelOption.SIDE);
                            response = CallResponse.createSuccessResponse("已切换到辅路");
                        } else {
                            response = CallResponse.createNotSupportResponse("当前已在辅路");
                            openMapWhenBackground();
                        }
                        break;
                    default:
                        return CallResponse.createFailResponse("不支持主路和辅路之外的选项");
                }
            } else {
                Logger.w(IVrBridgeConstant.TAG, "not int parallel status");
                response = CallResponse.createNotSupportResponse("当前没有主辅路可切换");
                openMapWhenBackground();
            }
        } else {
            Logger.w(IVrBridgeConstant.TAG, "not in navigation status");
            return CallResponse.createFailResponse("先发起导航才能切换主辅路");
        }

        response.setNeedPlayMessage(true);
        respTts(response, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 桥上和桥下切换
     *
     * @param bridge       ON：桥上；UNDER：桥下
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onBridgeChange(final String bridge, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onBridgeChange: bridge = " + bridge);
        CallResponse response = null;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final LocParallelInfoEntity parallelInfo = MapStateManager.getInstance().getParallelInfo();
            if (null != parallelInfo && parallelInfo.getStatus() == 1 && parallelInfo.getHwFlag() != 0) {
                final int hwFlag = parallelInfo.getFlag();
                switch (bridge) {
                    case IVrBridgeConstant.ParallelOption.ON:
                        //切换到高架上
                        if (hwFlag == 2) {
                            response = CallResponse.createSuccessResponse("已切换到桥上");
                            NaviPackage.getInstance().voiceChangeParallelRoad(MapType.MAIN_SCREEN_MAIN_MAP,
                                    IVrBridgeConstant.ParallelOption.ON);
                        } else {
                            response = CallResponse.createNotSupportResponse("当前已在桥上");
                            openMapWhenBackground();
                        }
                        break;
                    case IVrBridgeConstant.ParallelOption.UNDER:
                        //切换到高架下
                        if (hwFlag == 1) {
                            response = CallResponse.createSuccessResponse("已切换到桥下");
                            NaviPackage.getInstance().voiceChangeParallelRoad(MapType.MAIN_SCREEN_MAIN_MAP,
                                    IVrBridgeConstant.ParallelOption.UNDER);
                        } else {
                            response = CallResponse.createNotSupportResponse("当前已在桥下");
                            openMapWhenBackground();
                        }
                        break;
                    default:
                        return CallResponse.createFailResponse("不支持桥上和桥下以外的选项");
                }
            } else {
                Logger.w(IVrBridgeConstant.TAG, "not int parallel status");
                response = CallResponse.createNotSupportResponse("当前没有桥上下可切换");
                openMapWhenBackground();
            }
        } else {
            Logger.w(IVrBridgeConstant.TAG, "not in navigation status");
            return CallResponse.createFailResponse("先发起导航才能切换桥上下");
        }

        response.setNeedPlayMessage(true);
        respTts(response, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 前方如何行驶.
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onForwardAsk(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onForwardAsk:");
        final boolean isInNavi = Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING);
        return VoiceSearchManager.getInstance().handleForwardAsk(isInNavi, respCallback);
    }

    /**
     * 限速查询
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onSpeedLimitAsk(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onSpeedLimitAsk:");
        final CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final int limitSpeed = MapStateManager.getInstance().getLimitSpeed();
            if (limitSpeed > 0) {
                response = CallResponse.createSuccessResponse("当前道路限速" + limitSpeed + "km/h，请消息驾驶");
            } else {
                response = CallResponse.createSuccessResponse("前方暂无限速信息，不过也不要开的太快哦");
            }
        } else {
            return CallResponse.createFailResponse(" 当前不在导航状态，没有行程相关信息哦");
        }
        response.setNeedPlayMessage(true);
        respTts(response, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 红绿灯查询
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onTrafficLightAsk(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onTrafficLightAsk:");
        final CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final NaviEtaInfo etaInfo = MapStateManager.getInstance().getEtaInfo();
            if (null != etaInfo) {
                final int remainLightCount = etaInfo.routeRemainLightCount;
                Logger.d(IVrBridgeConstant.TAG, "trafficLightAsk count000000: " + remainLightCount);
                response = CallResponse.createSuccessResponse("离目的地剩余" + remainLightCount + "个红绿灯");
            } else {
                Logger.d(IVrBridgeConstant.TAG, "trafficLightAsk count11111111:");
                response = CallResponse.createSuccessResponse("暂无红绿灯信息，请稍后再试");
            }
        } else {
            Logger.d(IVrBridgeConstant.TAG, "trafficLightAsk count222222222:");
            return CallResponse.createFailResponse("当前不在导航状态，没有行程相关信息哦 ");
        }
        response.setNeedPlayMessage(true);
        respTts(response, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 剩余距离查询
     *
     * @param start        路线起点，可以为null
     * @param arrival      路线 终点，可以为null
     *                     PASSBY:途经点
     *                     DESTINATION：目的地/终点
     *                     commonPoi:指定poi
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onDistanceLeftAsk(@Nullable final String start, @Nullable final String arrival, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onDistanceLeftAsk: start = " + start + ", arrival = " + arrival);
        if (TextUtils.isEmpty(arrival)) {
            return CallResponse.createFailResponse("目的地不可为空");
        }

        final CallResponse response;
        switch (arrival) {
            case IVrBridgeConstant.PoiType.DESTINATION:
            case IVrBridgeConstant.PoiType.PASS_BY:
                Logger.d(IVrBridgeConstant.TAG, "distanceLeftAsk00000");
                response = getRemainDistance(arrival, respCallback);
                break;
            case IVrBridgeConstant.DestType.HOME:
                Logger.d(IVrBridgeConstant.TAG, "distanceLeftAsk11111");
                //查询到家的ETA信息
                response = getHomeEtaInfo(start, respCallback);
                break;
            case IVrBridgeConstant.DestType.COMPANY:
                Logger.d(IVrBridgeConstant.TAG, "distanceLeftAsk22222");
                //查询到公司的ETA信息
                response = getCompanyEtaInfo(start, respCallback);
                break;
            default:
                Logger.d(IVrBridgeConstant.TAG, "distanceLeftAsk3333 start: " + start + ",arrival: " + arrival);
                response = VoiceSearchManager.getInstance().getTwoPoiEtaInfo(start, arrival, respCallback);
                break;
        }

        return response;
    }

    /**
     * 获取目的地或途径点的剩余距离
     *
     * @param arrival      目的地
     * @param respCallback 结果异步回调.
     * @return CallResponse，执行结果回复.
     */
    private CallResponse getRemainDistance(final String arrival, final RespCallback respCallback) {
        final CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final NaviEtaInfo naviEtaInfo = MapStateManager.getInstance().getEtaInfo();
            if (null != naviEtaInfo) {
                if (IVrBridgeConstant.PoiType.DESTINATION.equals(arrival)) {
                    //目的地
                    final int remainDistance = naviEtaInfo.getAllDist();
                    final String formatDistances = VoiceConvertUtil.formatDistance(remainDistance);
                    Logger.d(IVrBridgeConstant.TAG, "distanceLeftAsk44444, formatDistance: " + formatDistances);
                    response = CallResponse.createSuccessResponse("距目的地还有" + formatDistances);
                } else {
                    //途径点
                    if (null != naviEtaInfo.viaRemain && !naviEtaInfo.viaRemain.isEmpty()) {
                        final NaviEtaInfo.NaviTimeAndDist timeAndDist = naviEtaInfo.viaRemain.get(0);
                        final int viaRemainDist = timeAndDist.dist;
                        final String formatDistances = VoiceConvertUtil.formatDistance(viaRemainDist);
                        Logger.d(IVrBridgeConstant.TAG, "distanceLeftAsk55555, formatDistance: " + formatDistances);
                        response = CallResponse.createSuccessResponse("距第一个途径点还有" + formatDistances);
                    } else {
                        response = CallResponse.createNotSupportResponse("没有查询到相关信息，请稍后重试");
                    }
                }
            } else {
                response = CallResponse.createNotSupportResponse("没有查询到相关新，请稍后重试 ");
            }
        } else {
            return CallResponse.createFailResponse(" 当前不在导航状态，没有行程相关信息哦");
        }
        response.setNeedPlayMessage(true);
        respTts(response, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 统一回复无网络状态.
     *
     * @return CallResponse.
     */
    private CallResponse createNoneNetwork() {
        return CallResponse.createFailResponse("当前网路状态不佳，请稍后再试");
    }

    /**
     * 获取到家的信息
     *
     * @param start        起始点
     * @param respCallback respCallback.
     * @return CallResponse，执行回复.
     */
    private CallResponse getHomeEtaInfo(final String start, final RespCallback respCallback) {
        final boolean networkStatus = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
        if (!networkStatus) {
            return createNoneNetwork();
        }
        final PoiInfoEntity homeInfo = BehaviorPackage.getInstance().getHomeFavoriteInfo();
        if (null == homeInfo) {
            Logger.e(IVrBridgeConstant.TAG, "have not save homeInfo");
            return CallResponse.createFailResponse("未找到家的地址，先去添加吧");
        }

        if (TextUtils.isEmpty(start)) {
            //以当前位置为起点
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null != locInfoBean) {
                final GeoPoint startPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                final GeoPoint endPoint = homeInfo.getPoint();
                final StringBuilder builder = new StringBuilder("到家有");
                processHomeCompanyEta(startPoint, endPoint, builder, respCallback);
            }
        } else {
            //指定地点回家的距离和时间
            VoiceSearchManager.getInstance().searchPoiInfo(IVrBridgeConstant.VoiceSearchType.TIME_AND_DIST,
                    IVrBridgeConstant.DestType.HOME, start, respCallback);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 获取到公司的ETA信息
     *
     * @param start        起始点
     * @param respCallback respCallback
     * @return CallResponse.
     */
    private CallResponse getCompanyEtaInfo(final String start, final RespCallback respCallback) {
        final boolean networkStatus = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
        if (!networkStatus) {
            return createNoneNetwork();
        }
        final PoiInfoEntity companyInfo = BehaviorPackage.getInstance().getCompanyFavoriteInfo();
        if (null == companyInfo) {
            Logger.e(IVrBridgeConstant.TAG, "have not save homeInfo");
            return CallResponse.createFailResponse("未找到家的地址，先去添加吧");
        }

        if (TextUtils.isEmpty(start)) {
            //以当前位置为起点
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null != locInfoBean) {
                final GeoPoint startPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                final GeoPoint endPoint = companyInfo.getPoint();
                final StringBuilder builder = new StringBuilder("到公司有");
                processHomeCompanyEta(startPoint, endPoint, builder, respCallback);
            }
        } else {
            //指定地点去公司的距离和时间
            VoiceSearchManager.getInstance().searchPoiInfo(IVrBridgeConstant.VoiceSearchType.TIME_AND_DIST,
                    IVrBridgeConstant.DestType.COMPANY, start, respCallback);
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * .
     *
     * @param start        start
     * @param end          end
     * @param builder      builder
     * @param respCallback respCallback
     */
    private void processHomeCompanyEta(final GeoPoint start, final GeoPoint end, final StringBuilder builder, final RespCallback respCallback) {
        RoutePackage.getInstance().getTravelTimeFuture(start, end)
                .thenAccept(pair -> {
                    final String distance = pair.first;
                    final String time = pair.second;
                    builder.append(distance).append("，大约需要").append(time);
                    final String homeCompanyEta = builder.toString();
                    Logger.d(IVrBridgeConstant.TAG, "homeCompanyEta: " + homeCompanyEta);
                    final CallResponse successResponse = CallResponse.createSuccessResponse(homeCompanyEta);
                    successResponse.setNeedPlayMessage(true);
                    respTts(successResponse, respCallback);
                })
                .exceptionally(throwable -> {
                    final CallResponse notSupportResponse = CallResponse.createNotSupportResponse("没有查询到相关信息，试试别的吧");
                    notSupportResponse.setNeedPlayMessage(true);
                    respTts(notSupportResponse, respCallback);
                    return null;
                });
    }

    /**
     * 剩余时长查询
     *
     * @param start        路线起点，可以为null
     * @param arrival      路线终点，可以为null
     *                     PASSBY:途经点
     *                     DESTINATION：目的地/终点
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onTimeLeftAsk(@Nullable final String start, @Nullable final String arrival, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onTimeLeftAsk: start = " + start + ", arrival = " + arrival);
        if (TextUtils.isEmpty(arrival)) {
            return CallResponse.createFailResponse("目的地不可为空");
        }

        final CallResponse response;
        switch (arrival) {
            case IVrBridgeConstant.PoiType.DESTINATION:
            case IVrBridgeConstant.PoiType.PASS_BY:
                Logger.d(IVrBridgeConstant.TAG, "timeLeftAsk0000000");
                response = getRemainTime(arrival, respCallback);
                break;
            case IVrBridgeConstant.DestType.HOME:
                Logger.d(IVrBridgeConstant.TAG, "timeLeftAsk1111111");
                //获取到家的ETA信息
                response = getHomeEtaInfo(start, respCallback);
                break;
            case IVrBridgeConstant.DestType.COMPANY:
                //获取到公司的ETA信息
                Logger.d(IVrBridgeConstant.TAG, "timeLeftAsk2222222");
                response = getCompanyEtaInfo(start, respCallback);
                break;
            default:
                Logger.d(IVrBridgeConstant.TAG, "timeLeftAsk33333333 start: " + start + ", arrival: " + arrival);
                response = VoiceSearchManager.getInstance().getTwoPoiEtaInfo(start, arrival, respCallback);
                break;
        }

        return response;
    }

    /**
     * 查询到目的地或途径点的剩余时间
     *
     * @param arrival      目的地
     * @param respCallback 执行结果异步回调.
     * @return CallResponse，直接结果回复.
     */
    private CallResponse getRemainTime(final String arrival, final RespCallback respCallback) {
        final CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final NaviEtaInfo naviEtaInfo = MapStateManager.getInstance().getEtaInfo();
            if (null != naviEtaInfo) {
                if (IVrBridgeConstant.PoiType.DESTINATION.equals(arrival)) {
                    //目的地
                    final String formatTime = VoiceConvertUtil.formatTime(naviEtaInfo.getAllTime());
                    Logger.d(IVrBridgeConstant.TAG, "timeLeftAsk4444 formatTime: " + formatTime);
                    response = CallResponse.createSuccessResponse("到目的地大约需要" + formatTime);
                } else {
                    //第一个途径点
                    if (null != naviEtaInfo.viaRemain && !naviEtaInfo.viaRemain.isEmpty()) {
                        final NaviEtaInfo.NaviTimeAndDist timeAndDist = naviEtaInfo.viaRemain.get(0);
                        final String viaRemainTime = VoiceConvertUtil.formatTime(timeAndDist.time);
                        Logger.d(IVrBridgeConstant.TAG, "timeLeftAsk555555 formatTime: " + viaRemainTime);
                        response = CallResponse.createSuccessResponse("距第一个途径点大约需要" + viaRemainTime);
                    } else {
                        response = CallResponse.createNotSupportResponse("没有查询到相关新，请稍后重试");
                    }
                }
            } else {
                response = CallResponse.createNotSupportResponse("没有查询到相关新，请稍后重试");
            }
        } else {
            return CallResponse.createFailResponse("当前不在导航状态，没有行程相关信息哦");
        }

        response.setNeedPlayMessage(true);
        respTts(response, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 路况查询（前方/道路/地点）
     *
     * @param trafficAskBean private String poi : 查询的地点、道路
     *                       private String start : 查询的起点
     *                       private String arrival : 查询的终点
     * @param respCallback   异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onTrafficConditionAsk(final TrafficAskBean trafficAskBean, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: trafficAskBean = " + trafficAskBean);
        return VoiceSearchManager.getInstance().handleTrafficConditionAsk(trafficAskBean, respCallback);
    }

    /**
     * 添加途径点
     *
     * @param sessionId   用来确保本轮数据的一致性
     * @param poi         待添加途径点名称
     * @param poiType     参数说明同ArrivalBean.destType
     * @param poiCallback poiCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onPassbyAdd(final String sessionId, final String poi, final String poiType, final PoiCallback poiCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onPassByAdd: sessionId = " + sessionId + ", poi = " + poi + ", poiType = " + poiType);
        if (TextUtils.isEmpty(sessionId) || TextUtils.isEmpty(poi)) {
            Logger.e(IVrBridgeConstant.TAG, "session or passBy is empty");
            return CallResponse.createFailResponse("沿途搜参数为空");
        }

        if (!MapStateManager.getInstance().isNaviStatus()) {
            //非导航态不支持沿途搜
            Logger.w(IVrBridgeConstant.TAG, "alongSearch in no navigation");
            return CallResponse.createFailResponse("需要发起导航，才能帮你规划沿途的路线，试试说：导航回家");
        }
        final RouteCurrentPathParam pathParam = RoutePackage.getInstance().getCurrentPathInfo(MapType.MAIN_SCREEN_MAIN_MAP);
        if (null != pathParam && pathParam.isMIsOnlineRoute()) {
            //离线算路不支持沿途搜
            Logger.w(IVrBridgeConstant.TAG, "alongSearch in offline road");
            return CallResponse.createFailResponse("当前使用离线算路，不支持该功能");
        }

        VoiceSearchManager.getInstance().handlePassBy(sessionId, poi, poiType, poiCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 删除途径点，存在多个途径点情况，需要知道途径点数据
     *
     * @param sessionId   用来确保本轮数据的一致性
     * @param poiCallback 返回多个途径点数据
     * @return CallResponse
     */
    @Override
    public CallResponse onPassbyDelete(final String sessionId, final PoiCallback poiCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onPassByDelete: sessionId = " + sessionId);
        return responseNotSupport();
    }

    /**
     * 删除途径点
     *
     * @param idx          待删除途径点位置
     *                     Integer.MAX_VALUE：代表全部
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onPassbyDelete(final int idx, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onPassByDelete: idx = " + idx);
        return responseNotSupport();
    }

    /**
     * 切换导航播报模式
     *
     * @param mode         BRIEF：简洁/专家模式；
     *                     DETAILED：详细/新手模式；
     *                     BEEP：提示音模式
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onBroadcastSwitch(final String mode, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onBroadcastSwitch: mode = " + mode);
        openMapWhenBackground();

        final CallResponse callResponse;
        //映射设置功能包
        final int broadcastMode;
        //用于tts播报
        final String tts;
        switch (mode) {
            case IVrBridgeConstant.VoiceBroadcastMode.BRIEF:
                broadcastMode = NaviConstant.BroadcastType.BROADCAST_CONCISE;
                tts = "简洁播报模式";
                break;
            case IVrBridgeConstant.VoiceBroadcastMode.DETAILED:
                broadcastMode = NaviConstant.BroadcastType.BROADCAST_DETAIL;
                tts = "详细播报模式";
                break;
            case IVrBridgeConstant.VoiceBroadcastMode.MINIMALIST:
                broadcastMode = NaviConstant.BroadcastType.BROADCAST_MINIMALISM;
                tts = "极简模式";
                break;
            default:
                Logger.d(IVrBridgeConstant.TAG, "Go default case, no mode match " + mode + "  currently, return !");
                return CallResponse.createFailResponse("不支持的播报模式");
        }

        final int curBroadcastMode = SettingPackage.getInstance().getConfigKeyBroadcastMode();
        Logger.i(IVrBridgeConstant.TAG, "curBroadcast: " + curBroadcastMode + ", target: " + broadcastMode);
        if (broadcastMode == curBroadcastMode) {
            callResponse = CallResponse.createNotSupportResponse("当前已为" + tts);
        } else {
            final int themeMode = SettingPackage.getInstance().getConfigKeyDayNightMode();
            NaviPackage.getInstance().updateBroadcastParam(
                    broadcastMode, IVrBridgeConstant.ThemeMode.DAY == themeMode);
            callResponse = CallResponse.createSuccessResponse("已切换为" + tts);
        }
        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 切换视图
     *
     * @param mode         2D_FOLLOW_LOGO：2D车头朝上;
     *                     2D：正北/正北朝上/正北向上/2D;
     *                     3D：车头朝上/车头向上/跟随车头/跟随/3D
     *                     DEFAULT：根据当前模式切换
     * @param tts          xx模式
     * @param respCallback respCallback
     * @return CallResponse，结果回复.
     */
    @Override
    public CallResponse onMapViewSwitch(final String mode, final String tts, final RespCallback respCallback) {

        openMapWhenBackground();
        final MapMode curMapMode = MapPackage.getInstance().getCurrentMapMode(MapType.MAIN_SCREEN_MAIN_MAP);
        Logger.d(IVrBridgeConstant.TAG, "onMapViewSwitch mode:" + mode + ", tts:" + tts + ", curMode:" + curMapMode.name());

        final String voiceTargetMode;
        final CallResponse callResponse;
        final MapMode targetMode;
        final String respTts;

        if (IVrBridgeConstant.VoiceMapMode.DEFAULT.equals(mode)) {
            //切换为下一个MapMode
            switch (curMapMode) {
                case UP_2D: //当前为2D车头朝上
                    voiceTargetMode = IVrBridgeConstant.VoiceMapMode.NORTH_2D;
                    break;
                case NORTH_2D: //当前为2D正北朝上
                    voiceTargetMode = IVrBridgeConstant.VoiceMapMode.CAR_3D;
                    break;
                case UP_3D: //当前为3D模式
                    voiceTargetMode = IVrBridgeConstant.VoiceMapMode.CAR_2D;
                    break;
                default:
                    voiceTargetMode = null;
                    break;
            }
        } else {
            voiceTargetMode = mode;
        }

        switch (voiceTargetMode) {
            case IVrBridgeConstant.VoiceMapMode.NORTH_2D:
                targetMode = MapMode.NORTH_2D;
                respTts = "正北模式";
                break;
            case IVrBridgeConstant.VoiceMapMode.CAR_2D:
                targetMode = MapMode.UP_2D;
                respTts = "2D模式";
                break;
            case IVrBridgeConstant.VoiceMapMode.CAR_3D:
                targetMode = MapMode.UP_3D;
                respTts = tts;
                break;
            default:
                return CallResponse.createFailResponse("不支持的地图模式");
        }

        if (targetMode == curMapMode) {
            callResponse = CallResponse.createNotSupportResponse("当前已是" + respTts);
        } else {
            Logger.d(IVrBridgeConstant.TAG, "Map view switch successfully !!! ");
            MapPackage.getInstance().switchMapMode(MapType.MAIN_SCREEN_MAIN_MAP, targetMode);
            callResponse = CallResponse.createSuccessResponse("已切换为" + respTts);
        }
        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, respCallback);

        return CallResponse.createSuccessResponse();
    }

    /**
     * 收藏位置
     *
     * @param poi          待收藏位置
     *                     CURRENT_LOCATION：当前定位；
     *                     DESTINATION：目的地/终点
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onFavoriteAdd(final String poi, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onFavoriteAdd: poi = " + poi);
        if (TextUtils.isEmpty(poi)) {
            Logger.e(IVrBridgeConstant.TAG, "addFavorite poi is empty");
            return CallResponse.createFailResponse("收藏目的地为空");
        }
        openMapWhenBackground();

        if (IVrBridgeConstant.PoiType.DESTINATION.equals(poi)) {
            //引导目的地
            final RouteParam routeParam = RoutePackage.getInstance().getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
            if (null != routeParam) {
                final PoiInfoEntity poiInfo = getPoiInfoEntity(routeParam);
                BehaviorPackage.getInstance().addFavorite(poiInfo, 0);
                final StringBuilder builder = new StringBuilder("已收藏目的地");
                final String name = poiInfo.getName();
                final String address = poiInfo.getAddress();
                if (!TextUtils.isEmpty(name)) {
                    builder.append(name);
                } else if (!TextUtils.isEmpty(address)) {
                    builder.append(address);
                }
                final CallResponse destResponse = CallResponse.createSuccessResponse(builder.toString());
                destResponse.setNeedPlayMessage(true);
                respTts(destResponse, respCallback);
            } else {
                return CallResponse.createFailResponse("获取目的地信息失败");
            }
        } else if (IVrBridgeConstant.PoiType.CURRENT_LOCATION.equals(poi)) {
            //当前定位
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            final double lon = null != locInfoBean ? locInfoBean.getLongitude() : 0.0;
            final double lat = null != locInfoBean ? locInfoBean.getLatitude() : 0.0;
            if (null == locInfoBean || Double.compare(lon, 0.0) <= 0 || Double.compare(lat, 0.0) <= 0) {
                Logger.e(IVrBridgeConstant.TAG, "curLocation is empty");
                return CallResponse.createFailResponse("不好意思，无法获取当前定位信息");
            } else {
                //获取当前定位详情
                Logger.d(IVrBridgeConstant.TAG, "addFavorite currentLocation lon: " + lon + ", lat: " + lat);
                final GeoPoint geoPoint = new GeoPoint(lon, lat);
                VoiceSearchManager.getInstance().queryCurrentLocationDetail(IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE,
                        geoPoint, respCallback);
            }
        } else {
            return CallResponse.createFailResponse("不支持的收藏类型");
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * getPoiInfoEntity
     *
     * @param routeParam routeParam
     * @return PoiInfoEntity
     */
    @NonNull
    private PoiInfoEntity getPoiInfoEntity(final RouteParam routeParam) {
        final PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setPid(routeParam.getPoiID());
        poiInfo.setName(routeParam.getName());
        poiInfo.setAddress(routeParam.getAddress());
        poiInfo.setAdCode(routeParam.getAdCode());
        if (null != routeParam.getRealPos()) {
            poiInfo.setPoint(routeParam.getRealPos());
        }
        final FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setCommonName(0);
        poiInfo.setFavoriteInfo(favoriteInfo);
        return poiInfo;
    }

    /**
     * 打开地图收藏夹
     *
     * @param respCallback respCallback.
     * @return CallResponse.
     */
    @Override
    public CallResponse onFavoriteOpen(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onFavoriteOpen:");
        openMapWhenBackground();
        final boolean inNavigation = MapStateManager.getInstance().isNaviStatus();
        if (inNavigation) {
            return CallResponse.createFailResponse("导航中无法打开收藏地址");
        } else {
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.FAVORITE_PAGE);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            final CallResponse callResponse = CallResponse.createSuccessResponse("已打开收藏地址");
            callResponse.setNeedPlayMessage(true);
            respTts(callResponse, respCallback);
            return CallResponse.createSuccessResponse();
        }
    }

    /**
     * 打开导航搜索记录
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onSearchListOpen(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onSearchListOpen:");
        openMapWhenBackground();

        final boolean inNavigation = MapStateManager.getInstance().isNaviStatus();
        if (inNavigation) {
            return CallResponse.createFailResponse("导航中无法打开历史记录");
        } else {
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.SEARCH_HISTORY);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            final CallResponse callResponse = CallResponse.createSuccessResponse("已打开历史记录");
            callResponse.setNeedPlayMessage(true);
            respTts(callResponse, respCallback);
            return CallResponse.createSuccessResponse();
        }
    }

    /**
     * POI列表选择
     *
     * @param sessionId    用来确保本轮数据的一致性
     * @param type         条件类型
     *                     distance：距离，
     *                     price：价格，
     *                     rate：评价，
     *                     position：位置
     * @param typeValue    条件值
     *                     distance = NEAREST：最近的
     *                     price = DEAREST：最贵的；CHEAPEST：最便宜的
     *                     rate = HIGHEST：评价最高的
     *                     position => 1
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onPoiSelect(final String sessionId, final String type, final String typeValue, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onPoiSelect: sessionId = " + sessionId + ", Type:" + type + ", typeValue:" + typeValue);
        boolean precess = true;
        switch (type) {
            case IVrBridgeConstant.PoiSelectType.POSITION:
                final int index = Integer.parseInt(typeValue);
                VoiceSearchManager.getInstance().handlePoiSelectIndex(sessionId, Math.max(index - 1, 0), respCallback);
                break;
            case IVrBridgeConstant.PoiSelectType.DISTANCE:
                if ("NEAREST".equals(typeValue)) {
                    VoiceSearchManager.getInstance().handlePoiSelectRule(sessionId, 1, respCallback);
                } else {
                    precess = false;
                }
                break;
            case IVrBridgeConstant.PoiSelectType.RATE:
                if ("HIGHEST".equals(typeValue)) {
                    VoiceSearchManager.getInstance().handlePoiSelectRule(sessionId, 2, respCallback);
                } else {
                    precess = false;
                }
                break;
            case IVrBridgeConstant.PoiSelectType.PRICE:
                if ("CHEAPEST".equals(typeValue)) {
                    VoiceSearchManager.getInstance().handlePoiSelectRule(sessionId, 3, respCallback);
                } else if ("DEAREST".equals(typeValue)) {
                    VoiceSearchManager.getInstance().handlePoiSelectRule(sessionId, 4, respCallback);
                } else {
                    precess = false;
                }
                break;
            default:
                precess = false;
                break;
        }

        if (!precess) {
            final CallResponse callResponse = CallResponse.createNotSupportResponse("不支持的筛选类型");
            callResponse.setNeedPlayMessage(true);
            respTts(callResponse, respCallback);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * POI排序
     *
     * @param sessionId    用来确保本轮数据的一致性
     * @param type         条件类型
     *                     DISTANCE：距离，
     *                     PRIZE：价格，
     *                     RATE：评价
     * @param rule         条件值
     *                     ASCENDING：升序；
     *                     DESCENDING：降序
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onPoiSort(final String sessionId, final String type, final String rule, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onPoiSort: sessionId = " + sessionId + ", type = " + type + ", rule = " + rule);
        VoiceSearchManager.getInstance().sortPoi(sessionId, type, rule, respCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * POI页面选择
     *
     * @param sessionId    多轮时，保证数据一致性
     * @param type         页面选择类型
     *                     direction：方位，比如上一页、下一页
     *                     index：正向位置，比如第一页
     *                     index_reverse：反向位置，比如倒数第一页，最后一页
     * @param typeValue    页面选择类型的值
     *                     direction = UP:上一页；DOWN：下一页
     *                     index、index_reverse >= 1，具体页码值
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onPoiPageChange(final String sessionId, final String type, final String typeValue, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onPoiPageChange: sessionId = " + sessionId + ", type = " + type + ", typeValue = " + typeValue);
        return CallResponse.createFailResponse("暂不支持此功能");
    }

    /**
     * 导航多轮中断退出
     *
     * @param type 默认 DEFAULT。预留扩展，后续支持多种退出类型
     * @return CallResponse
     */
    @Override
    public CallResponse onMultiRoundExit(final String type) {
        Logger.d(IVrBridgeConstant.TAG, "onMultiRoundExit: type = " + type);
        return CallResponse.createFailResponse("暂不支持");
    }

    /**
     * 打开和关闭地图
     *
     * @param action       OPEN: 打开; CLOSE: 关闭
     * @param target       BAIDU_MAP：百度地图；
     *                     A_MAP：高德地图；
     *                     GEELY_MAP：吉利地图
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onMapToggle(final String action, final String target, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onMapToggle: action = " + action + ", target = " + target);
        switch (action) {
            case IVrBridgeConstant.MapToggleAction.OPEN:
                openMap();
                break;
            case IVrBridgeConstant.MapToggleAction.CLOSE:
                closeMap(respCallback);
                break;
            default:
                return CallResponse.createFailResponse("不支持的操作类型");
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 当HMI处于后台，切换到前台
     */
    private void openMapWhenBackground() {
        if (!NaviPackage.getInstance().getIsAppInForeground()) {
            openMap();
        }
    }

    /**
     * 切换到前台
     */
    private void openMap() {
        if (null != AppContext.getInstance().getMContext()) {
            try {
                final String appPkgName = AppContext.getInstance().getMContext().getPackageName();
                final PackageManager packageManager = AppContext.getInstance().getMContext().getPackageManager();
                final Intent launcherIntent = packageManager.getLaunchIntentForPackage(appPkgName);
                if (null != launcherIntent) {
                    launcherIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    AppContext.getInstance().getMContext().startActivity(launcherIntent);
                } else {
                    Logger.e(IVrBridgeConstant.TAG, "can't find map hmi");
                }
            } catch (ActivityNotFoundException exception) {
                Logger.e(IVrBridgeConstant.TAG, "open map error: " + exception.getMessage());
            }
        }
    }

    /**
     * 关闭地图
     *
     * @param respCallback 执行结果回调.
     */
    private void closeMap(final RespCallback respCallback) {
        if (MapStateManager.getInstance().isNaviStatus()) {
            stopNavigation();
        }
        VoiceSearchManager.getInstance().sendMoveBack();
        if (null != respCallback) {
            final CallResponse response = CallResponse.createSuccessResponse("已为你关闭地图");
            response.setNeedPlayMessage(true);
            respCallback.onResponse(response);
        }
    }

    /**
     * 修改日夜模式
     *
     * @param mode         1: 白天模式; 2: 黑夜模式;
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onDayNightModeChange(final int mode, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onDayNightModeChange: mode = " + mode);
        return CallResponse.createFailResponse("不支持的功能");
    }

    /**
     * 打开关闭登录页
     *
     * @param toggle       true: 打开; false: 关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onLoginPageToggle(final boolean toggle, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onLoginPageToggle: toggle = " + toggle);
        return CallResponse.createFailResponse("不支持此功能");
    }

    /**
     * 创建队伍
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamCreate(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onNaviTeamCreate:");
        return CallResponse.createFailResponse("不支持组队相关功能");
    }

    /**
     * 退出队伍
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamQuit(final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onNaviTeamQuit:");
        return CallResponse.createFailResponse("不支持组队相关功能");
    }

    /**
     * 打开关闭队伍全览
     *
     * @param toggle       true: 打开; false: 关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamPageToggle(final boolean toggle, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onNaviTeamPageToggle: toggle = " + toggle);
        return CallResponse.createFailResponse("不支持组队相关功能");
    }

    /**
     * 打开关闭组队出行协议页面
     *
     * @param toggle       true: 打开; false: 关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamAgreementPageToggle(final boolean toggle, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onNaviTeamAgreementPageToggle: toggle = " + toggle);
        return CallResponse.createFailResponse("不支持组队相关功能");
    }

    /**
     * 加入队伍
     *
     * @param code         队伍口令
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamJoin(final String code, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onNaviTeamJoin: code = " + code);
        return CallResponse.createFailResponse("not support team function");
    }

    /**
     * 静音开关
     *
     * @param open     true：静音 false：关闭静音
     * @param callback 回调
     * @return CallResponse
     */
    @Override
    public CallResponse onMuteToggle(final boolean open, final RespCallback callback) {
        openMapWhenBackground();

        final int muteStatus = SettingPackage.getInstance().getConfigKeyMute();
        Logger.d(IVrBridgeConstant.TAG, "onMuteToggle: open = " + open + ", current: " + muteStatus);
        final CallResponse callResponse;
        if (open) {
            if (muteStatus == 1) {
                callResponse = CallResponse.createNotSupportResponse("当前导航声音已关闭");
            } else {
                NaviPackage.getInstance().setMute(true);
                callResponse = CallResponse.createSuccessResponse("已关闭导航声音");
            }
        } else {
            if (muteStatus == 0) {
                callResponse = CallResponse.createNotSupportResponse("当前导航声音已打开");
            } else {
                NaviPackage.getInstance().setMute(false);
                callResponse = CallResponse.createSuccessResponse("已打开导航声音");
            }
        }
        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, callback);

        return CallResponse.createSuccessResponse();
    }

    @Override
    public CallResponse onVolumeAdjust(final NaviControlParam naviControlParam, final RespCallback callback) {
        return CallResponse.createFailResponse("地图不支持此功能");
    }

    /**
     * 判空后回复
     *
     * @param callResponse 结果与tts
     * @param respCallback respCallback
     */
    private void respTts(final CallResponse callResponse, final RespCallback respCallback) {
        if (null != respCallback) {
            respCallback.onResponse(callResponse);
        }
    }
}