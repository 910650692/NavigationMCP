package com.sgm.navi.vrbridge.impl;

import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;

import com.android.utils.thread.ThreadManager;
import com.baidu.oneos.protocol.bean.ArrivalBean;
import com.baidu.oneos.protocol.bean.CallResponse;
import com.baidu.oneos.protocol.bean.PoiBean;
import com.baidu.oneos.protocol.bean.TrafficAskBean;
import com.baidu.oneos.protocol.bean.param.NaviControlParam;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.callback.RespCallback;
import com.baidu.oneos.protocol.listener.NaviControlCommandListener;
import com.baidu.oneos.protocol.result.NaviSubCallResult;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviExchangeEntity;
import com.sgm.navi.service.define.navi.RoadName;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.position.LocParallelInfoEntity;
import com.sgm.navi.service.define.route.RouteCurrentPathParam;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.route.RouteLineSegmentInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePreferenceID;
import com.sgm.navi.service.define.route.RoutePriorityType;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RouteWayID;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.service.utils.ExportIntentParam;
import com.sgm.navi.vrbridge.IVrBridgeConstant;
import com.sgm.navi.vrbridge.MapStateManager;
import com.sgm.navi.vrbridge.VoiceConvertUtil;
import com.sgm.navi.vrbridge.bean.SingleCommandInfo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class NaviControlCommandImpl implements NaviControlCommandListener {

    //保存个性化道路上一轮找到的路线信息 start
    private NaviExchangeEntity.NewRoute mNewRoute = null;
    //保存下一轮指令的列表，需要在Map从未打开到底图加载完成后继续执行
    private List<String> mCommandList = new ArrayList<>();
    //保存下一轮指令对应的参数
    private List<SingleCommandInfo> mCommandParamList = new ArrayList<>();

    //用于路况查询
    private int mTrafficConditionResult = -2;//-1：无数据 0:未知状态 1:通畅 2:缓慢 3:拥堵 4:严重拥堵 5:极度通畅
    private boolean mIsInRoute = false;//判断点位是否是导航线路上的
    private boolean mHasProcessed = false;//判断有没有处理
    private String mTtsContentForCondition = "";//回复内容
    private List<RouteParam> mAllPoiParamList;
    private Map<RouteParam, String> mNameMap;


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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onMapSizeAdjust: zoomIn = ", zoomIn, ", size = ", size);
        }

        final boolean saveCommand = MapStateManager.getInstance().openMapForRestoreCommand();
        final CallResponse callResponse;
        final float curSize = MapPackage.getInstance().getZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        final float levelSize = getZoomLevelBySize(size);
        if (Float.compare(0.0f, levelSize) > 0) {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_ZOOM_COMMAND);
        }

        if (zoomIn) {
            if (curSize == AutoMapConstant.MAP_ZOOM_LEVEL_MAX) {
                callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ALREADY_MAX_SIZE);
            } else {
                if (saveCommand) {
                    saveZoomCommand(true, size);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ZOOM_IN);
                } else if (levelSize == AutoMapConstant.MAP_ZOOM_LEVEL_MAX) {
                    //直接放到最大
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Set map level max");
                    }
                    MapPackage.getInstance().setZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP, levelSize);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ZOOM_TO_MAX);
                } else {
                    //默认flag提升
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Amplify map level");
                    }
                    MapPackage.getInstance().amplifyLevel(MapType.MAIN_SCREEN_MAIN_MAP);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ZOOM_IN);
                }
            }
        } else {
            if (curSize == AutoMapConstant.MAP_ZOOM_LEVEL_MIN) {
                callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ALREADY_MIN_SIZE);
            } else {
                if (saveCommand) {
                    saveZoomCommand(false, size);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ZOOM_OUT);
                } else if (levelSize == AutoMapConstant.MAP_ZOOM_LEVEL_MIN) {
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Set map level min");
                    }
                    MapPackage.getInstance().setZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP, levelSize);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ZOOM_TO_MIN);
                } else {
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Reduce map level");
                    }
                    MapPackage.getInstance().reduceLevel(MapType.MAIN_SCREEN_MAIN_MAP);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ZOOM_OUT);
                }
            }
        }
        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, respCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 根据语音指令参数获取Map缩放值.
     *
     * @param size int 语音指令传入的缩放值.
     * @return levelSize，Map使用的
     */
    private float getZoomLevelBySize(final int size) {
        final float levelSize = switch (size) {
            case Integer.MAX_VALUE -> AutoMapConstant.MAP_ZOOM_LEVEL_MAX;
            case Integer.MIN_VALUE -> AutoMapConstant.MAP_ZOOM_LEVEL_MIN;
            case 1 -> AutoMapConstant.MAP_ZOOM_LEVEL_CHANGE_FLAG;
            default -> {
                if (Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "Error zoom action !");
                }
                yield -1.0f;
            }
        };

        return levelSize;
    }

    /**
     * 保存语音缩放指令，在主图加载完成后再执行.
     *
     * @param zoomIn       true:放大; false:缩小
     * @param size         Integer.MAX_VALUE: 放大到最大；Integer.MIN_VALUE: 缩小到最小；1: 默认放大\缩小
     */
    private void saveZoomCommand(final boolean zoomIn, final int size) {
        ExportIntentParam.setIntentPage(IVrBridgeConstant.VrExportPage.ZOOM_LEVEL);
        ExportIntentParam.setKeyword( zoomIn ? "1" : "0");
        ExportIntentParam.setIntParam(size);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onRouteOverviewToggle: open = ", open);
        }
        final boolean inNavigation = MapStateManager.getInstance().isNaviStatus();
        final boolean preview = NaviPackage.getInstance().getPreviewStatus();
        final CallResponse callResponse;
        if (open) {
            //查看全程
            if (inNavigation) {
                if (preview) {
                    callResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_SHOW_PREVIEW);
                } else {
                    NaviPackage.getInstance().voiceRouteOverview(MapType.MAIN_SCREEN_MAIN_MAP, true);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.SHOW_PREVIEW);
                }
            } else {
                callResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NAVI_CANT_SHOW_PREVIEW);
            }
        } else {
            //退出全程
            if (inNavigation) {
                if (preview) {
                    NaviPackage.getInstance().voiceRouteOverview(MapType.MAIN_SCREEN_MAIN_MAP, false);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.EXIT_PREVIEW);
                } else {
                    callResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_EXIT_PREVIEW);
                }
            } else {
                MapStateManager.getInstance().openMapWhenBackground();
                callResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_EXIT_PREVIEW);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onClassicNaviToggle: open = ", open);
        }
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_LIGHT_NAVI);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onFamiliarNaviToggle: open = ", open);
        }
        return responseNotSupport();
    }

    /**
     * 不支持的功能统一回复.
     *
     * @return CallResponse.
     */
    private CallResponse responseNotSupport() {
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_THIS_FUNCTION);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onArNaviToggle: open = ", open);
        }
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onFamiliarNaviToggle:");
        }
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onNaviActionChange: action = ", action);
        }

        final CallResponse callResponse;
        switch (action) {
            case IVrBridgeConstant.NavigationOperateType.START:
                if (MapStateManager.getInstance().isNaviStatus()) {
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Already in navigation state");
                    }
                    MapStateManager.getInstance().openMapWhenBackground();
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ALREADY_IN_NAVI);
                } else if (MapStateManager.getInstance().inSelectRoute()) {
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Selecting route page, start navigation");
                    }
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.LETS_GO);
                    final Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.START_NAVIGATION);
                    MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                } else {
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Haven't select route, keep listen");
                    }
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.WHERE_TO_GO);
                }
                break;
            case IVrBridgeConstant.NavigationOperateType.STOP:
                if (MapStateManager.getInstance().isNaviStatus()) {
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Stop navigation successfully");
                    }
                    stopNavigation();
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.NAVI_END);
                } else {
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "Already end navigation");
                    }
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.NOT_IN_NAVI);
                }
                break;
            case IVrBridgeConstant.NavigationOperateType.CONTINUE:
                final int sceneState = NaviPackage.getInstance().getCurrentImmersiveStatus();
                final boolean previewStatus = NaviPackage.getInstance().getPreviewStatus();
                if (Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "Continue navigation sceneState: ", sceneState, "previewStatus:", previewStatus);
                }
                if (sceneState == 0 || previewStatus) {
                    //触碰态或全览态
                    NaviPackage.getInstance().voiceContinueNavigation(MapType.MAIN_SCREEN_MAIN_MAP);
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.RESUME_NAVI);
                } else {
                    // 不执行，直接按FeatureList返回提示
                    callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.OK);
                }
                break;
            default:
                if (Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "Error navi action");
                }
                callResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_NAVI_COMMAND);
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
        NaviPackage.getInstance().stopNavigation(true);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onEDogModeToggle: open = ", open);
        }
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_VR_CTRL);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onTrafficModeToggle open: ", open, ", curStatus: ", curTrafficMode);
        }
        final boolean saveCommand = MapStateManager.getInstance().openMapForRestoreCommand();

        final StringBuilder builder = new StringBuilder();
        if (open == curTrafficMode) {
            builder.append("当前").append(IVrBridgeConstant.ROAD_CONDITION).append("已").append(open ? "打开" : "关闭");
        } else {
            if (saveCommand) {
                ExportIntentParam.setIntentPage(IVrBridgeConstant.VrExportPage.ROAD_CONDITION);
                ExportIntentParam.setIntParam(open ? 1 : 0);
            } else {
                MapPackage.getInstance().setTrafficStates(MapType.MAIN_SCREEN_MAIN_MAP, open);
            }
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onRoadAssigned: road = ", road);
        }
        if (TextUtils.isEmpty(road)) {
            Logger.w(IVrBridgeConstant.TAG, "specialRoad empty");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.EMPTY_ROAD_NAME);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_AFTER_ADD_VIA);
    }

    /**
     * 不在选路态/引导态请先发起导航提示.
     *
     * @return CallResponse 提示信息回复.
     */
    private CallResponse responseStartNaviFirst() {
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.PLEASE_NAVI_FIRST);
    }

    /**
     * 选择某条路.
     *
     * @param roadName     选定道路名称.
     * @param respCallback RespCallback,语音异步回调.
     */
    private void chooseRoadWhenSelect(final String roadName, final RespCallback respCallback) {
        if (null == roadName || roadName.isEmpty()) {
            if (Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "chooseRoad name are empty");
            }
            return;
        }

        MapStateManager.getInstance().judgeFullScreen();
        boolean hasMatched = false;
        int chooseIndex = -1;
        final List<RouteLineInfo> routeLineList = MapStateManager.getInstance().getRouteList();
        try {
            final int size = routeLineList.size();
            outLoop:
            for (int i = 0; i < size; i++) {

                final RouteLineInfo lineInfo = routeLineList.get(i);
                if (null == lineInfo ) {
                    continue;
                }
                List<RouteLineSegmentInfo> segmentList = lineInfo.getMRouteLineSegmentInfos();
                if (null == segmentList || segmentList.isEmpty()) {
                    segmentList = RoutePackage.getInstance().voicePersonalizationRoute(routeLineList, i);
                }
                if (null == segmentList || segmentList.isEmpty()) {
                    return;
                }

                for (RouteLineSegmentInfo segmentInfo : segmentList) {
                    if (segmentInfo.getMLoadName().contains(roadName)) {
                        hasMatched = true;
                        chooseIndex = i;
                        break outLoop;
                    }
                }
            }
        } catch (NullPointerException exception) {
            Logger.e(IVrBridgeConstant.TAG, "match road error", exception.getMessage(), Arrays.toString(exception.getStackTrace()));
        } catch (IndexOutOfBoundsException outOfBoundsException) {
            Logger.e(IVrBridgeConstant.TAG, "match road index error", outOfBoundsException.getMessage(), Arrays.toString(outOfBoundsException.getStackTrace()));
        }
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "assignRoad chooseRoute: ", chooseIndex);
        }

        if (hasMatched) {
            try {
                responseSpecialRoad(true, roadName, routeLineList.get(chooseIndex), respCallback);
                final Map<MapType, Integer> indexMap = RoutePackage.getInstance().getSelectRouteIndex();
                final Integer curSelectValue = indexMap.getOrDefault(MapType.MAIN_SCREEN_MAIN_MAP, -1);
                final int mainIndex = null != curSelectValue ? curSelectValue : 0;
                if (Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "assignRoad curSelect: ", mainIndex);
                }
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
                Logger.w(IVrBridgeConstant.TAG, "match road, getSelect index empty", nullException.getMessage(), Arrays.toString(nullException.getStackTrace()));
            } catch (IndexOutOfBoundsException iobException) {
                Logger.w(IVrBridgeConstant.TAG, "match road, getSelect index outOfBound", iobException.getMessage(), Arrays.toString(iobException.getStackTrace()));
            }
        } else {
            final CallResponse response = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_FIND_WAY_TO_GO + roadName + IVrBridgeConstant.ResponseString.RATIONAL_ROUTE);
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
        MapStateManager.getInstance().judgeFullScreen();
        final NaviExchangeEntity naviExchangeEntity = NaviPackage.getInstance()
                .getExchangeResult(road, 1, MapType.MAIN_SCREEN_MAIN_MAP);
        final int changeType = naviExchangeEntity.getExchangeType();
        final StringBuilder builder = new StringBuilder();
        final CallResponse response;
        switch (changeType) {
            case NaviExchangeEntity.ExchangeType.ROAD_HAS_PASSED:
                mNewRoute = null;
                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.CURRENT_ROUTE_INCLUDE_ROAD);
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
                response.setSubCallResult(NaviSubCallResult.RESP_CONFIRM_AGAIN);
                break;
            default:
                mNewRoute = null;
                Logger.w(IVrBridgeConstant.TAG, "unHandle naviExchangeType: " + changeType);
                response = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_FIND_WAY_TO_GO + road + IVrBridgeConstant.ResponseString.INFORMATION);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onRoadNonAssigned: road = ", road);
        }
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
            if (Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "avoidRoad name are empty");
            }
            return;
        }

        MapStateManager.getInstance().judgeFullScreen();
        final List<RouteLineInfo> routeLineList = MapStateManager.getInstance().getRouteList();
        try {
            final int size = routeLineList.size();
            if (Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "不走xxx路，size: ", size);
            }
            final List<Boolean> matchedStatusList = new ArrayList<>();
            for (int i = 0; i < size; i++) {
                boolean matched = false;

                final RouteLineInfo lineInfo = routeLineList.get(i);
                if (null == lineInfo) {
                    continue;
                }
                List<RouteLineSegmentInfo> segmentList = lineInfo.getMRouteLineSegmentInfos();
                if (null == segmentList || segmentList.isEmpty()) {
                    segmentList = RoutePackage.getInstance().voicePersonalizationRoute(routeLineList, i);
                }
                if (null == segmentList || segmentList.isEmpty()) {
                    return;
                }

                for (RouteLineSegmentInfo segmentInfo : segmentList) {
                    if (segmentInfo.getMLoadName().contains(road)) {
                        matched = true;
                        break;
                    }
                }
                matchedStatusList.add(matched);
            }

            final Map<MapType, Integer> indexMap = RoutePackage.getInstance().getSelectRouteIndex();
            final Integer curSelectValue = indexMap.getOrDefault(MapType.MAIN_SCREEN_MAIN_MAP, -1);
            final int curIndex = null != curSelectValue ? curSelectValue : 0;
            if (Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "afterProcess matchStatusSize: ", matchedStatusList.size()
                        + ", currentSelectIndex: " + curIndex);
            }
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
                    final CallResponse response = CallResponse.createFailResponse(
                            IVrBridgeConstant.ResponseString.CANT_FIND_WAY_NOT_GO + road + IVrBridgeConstant.ResponseString.RATIONAL_ROUTE);
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
            Logger.e(IVrBridgeConstant.TAG, "avoid road error,", exception.getMessage(), Arrays.toString(exception.getStackTrace()));
        } catch (IndexOutOfBoundsException outOfBoundsException) {
            Logger.e(IVrBridgeConstant.TAG, "avoid road index error", outOfBoundsException.getMessage(), Arrays.toString(outOfBoundsException.getStackTrace()));
        }
    }

    /**
     * 引导态避开指定道路.
     *
     * @param road         String，指定道路名称.
     * @param respCallback RespCallback，执行结果回调.
     */
    private void avoidRoadWhenNavigation(final String road, final RespCallback respCallback) {
        MapStateManager.getInstance().judgeFullScreen();
        final NaviExchangeEntity naviExchangeEntity = NaviPackage.getInstance().getExchangeResult(road, 0, MapType.MAIN_SCREEN_MAIN_MAP);
        final int changeType = naviExchangeEntity.getExchangeType();
        final StringBuilder builder = new StringBuilder();
        final CallResponse response;
        switch (changeType) {
            case NaviExchangeEntity.ExchangeType.ROAD_HAS_PASSED:
                mNewRoute = null;
                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.CURRENT_ROUTE_INCLUDE_ROAD);
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
                response.setSubCallResult(NaviSubCallResult.RESP_CONFIRM_AGAIN);
                break;
            default:
                Logger.w(IVrBridgeConstant.TAG, "unHandle avoidRoad type: " + changeType);
                response = CallResponse.createFailResponse(
                        IVrBridgeConstant.ResponseString.CANT_FIND_WAY_NOT_GO + road + IVrBridgeConstant.ResponseString.INFORMATION);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onRouteRefresh:");
        }
        if (MapStateManager.getInstance().isNaviStatus()) {
            ThreadManager.getInstance().execute(() -> {
                final RouteRequestParam param = new RouteRequestParam();
                param.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
                param.setMRouteWay(RouteWayID.ROUTE_WAY_REFRESH);
                param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH);
                RoutePackage.getInstance().requestRoute(param);
                final CallResponse response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ALREADY_REFRESH_ROUTE);
                response.setNeedPlayMessage(true);
                respTts(response, respCallback);
            });
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.PLEASE_NAVI_FIRST);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onRouteSwitch:");
        }
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
        final CallResponse response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ALREADY_SWITCH_PREFERENCE);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onRoadChoose: road = ", type);
        }
        final CallResponse callResponse;

        //用于存入settingPackage
        final RoutePreferenceID routeId;
        //用于返回tts
        final String tts;
        switch (type) {
            case IVrBridgeConstant.RouteType.PREFER_RECOMMEND:
                routeId = RoutePreferenceID.PREFERENCE_RECOMMEND;
                tts = IVrBridgeConstant.ResponseString.PREFER_RECOMMEND;
                break;
            case IVrBridgeConstant.RouteType.NOT_HIGHWAY:
                routeId = RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
                tts = IVrBridgeConstant.ResponseString.NOT_HIGHWAY;
                break;
            case IVrBridgeConstant.RouteType.LESS_CHARGE:
                routeId = RoutePreferenceID.PREFERENCE_LESSCHARGE;
                tts = IVrBridgeConstant.ResponseString.LESS_CHARGE;
                break;
            case IVrBridgeConstant.RouteType.AVOID_JAM:
                routeId = RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
                tts = IVrBridgeConstant.ResponseString.AVOID_JAM;
                break;
            case IVrBridgeConstant.RouteType.FAST_SPEED:
                routeId = RoutePreferenceID.PREFERENCE_FASTESTSPEED;
                tts = IVrBridgeConstant.ResponseString.FAST_SPEED;
                break;
            case IVrBridgeConstant.RouteType.FIRST_HIGHWAY:
                routeId = RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
                tts = IVrBridgeConstant.ResponseString.FIRST_HIGHWAY;
                break;
            case IVrBridgeConstant.RouteType.MAIN_ROUTE:
                routeId = RoutePreferenceID.PREFERENCE_FIRSTMAINROAD;
                tts = IVrBridgeConstant.ResponseString.MAIN_ROUTE;
                break;
            default:
                if (Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "Go default case, no preference match currently, return !");
                }
                return CallResponse.createFailResponse(IVrBridgeConstant.RouteType.NOT_SUPPORT_TYPE);
        }

        MapStateManager.getInstance().openMapWhenBackground();
        final RoutePreferenceID curPrefer = SettingPackage.getInstance().getRoutePreference();
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "curRoutePrefer: ", curPrefer, ", target: ", routeId);
        }
        if (routeId == curPrefer) {
            callResponse = CallResponse.createSuccessResponse(
                    IVrBridgeConstant.ResponseString.ALREADY_IS + tts + IVrBridgeConstant.ResponseString.STH_ROUTE);
        } else {
            SettingPackage.getInstance().setRoutePreferenceByVoice(routeId);
            callResponse = CallResponse.createSuccessResponse(
                    IVrBridgeConstant.ResponseString.ALREADY_USE + tts + IVrBridgeConstant.ResponseString.PLAN);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "Route Choose by index : road = ", index);
        }
        if (MapStateManager.getInstance().inSelectRoute()) {
            final List<RouteLineInfo> routeLineInfoList = MapStateManager.getInstance().getRouteList();
            final int routeCount = null != routeLineInfoList ? routeLineInfoList.size() : 0;
            if (routeCount == 0) {
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ROUTE_RESULT_VOID);
            }
            final int routeIndex = index - 1;
            if (routeIndex < 0 || routeIndex >= routeCount) {
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_IN_RANGE);
            }

            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.SELECT_ROUTE);
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.ROUTE_INDEX, routeIndex);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            final CallResponse response = CallResponse.createSuccessResponse(
                    IVrBridgeConstant.ResponseString.ALREADY_CHOOSE + index + IVrBridgeConstant.ResponseString.ROUTE_NUM);
            response.setNeedPlayMessage(true);
            respCallback.onResponse(response);
            return CallResponse.createSuccessResponse();
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_CHOOSE_ROUTE);
        }
    }

    /**
     * 设置常用地址——家和公司或收藏指定poi.
     *
     * @param sessionId   用来确保本轮数据的一致性
     * @param poiType     HOME：家；COMPANY：公司 (此两个是设置家公司地址) poiCollect:收藏指定poi
     * @param poi         关键字
     * @param poiCallback poiCallback
     * @return CallResponse 语音执行结果回复.
     */
    @Override
    public CallResponse onCommonPoiSet(final String sessionId, final String poiType, @NonNull final String poi, final PoiCallback poiCallback) {
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onCommonPoiSet: sessionId = ", sessionId, ", poiType = ", poiType, ", poi = ", poi);
        }
        MapStateManager.getInstance().judgeFullScreen();
        final boolean saveCommand = MapStateManager.getInstance().openMapWhenBackground();
        if (saveCommand) {
            mCommandList.add(IVrBridgeConstant.VoiceCommandAction.COLLECT_COMMON);
            final SingleCommandInfo singleCommandInfo = new SingleCommandInfo();
            singleCommandInfo.setPoiName(poi);
            singleCommandInfo.setPoiType(poiType);
            singleCommandInfo.setPoiCallback(poiCallback);
            mCommandParamList.add(singleCommandInfo);
            return CallResponse.createSuccessResponse();
        } else {
            return processPoiSetCommand(sessionId, poiType, poi, poiCallback);
        }
    }

    //处理poiSet指令
    private CallResponse processPoiSetCommand(final String sessionId, final String poiType,
                                              @NonNull final String poi, final PoiCallback poiCallback) {
        switch (poiType) {
            case IVrBridgeConstant.DestType.POI_COLLECT:
                //普通收藏点
                return VoiceSearchManager.getInstance().searchForFavorite(poi, poiCallback);
            case IVrBridgeConstant.DestType.HOME:
            case IVrBridgeConstant.DestType.COMPANY:
                //设置家/公司地址
                return VoiceSearchManager.getInstance().setHomeCompany(sessionId, poiType, poi, poiCallback);
            case IVrBridgeConstant.DestType.NAVI_TO_HOME:
            case IVrBridgeConstant.DestType.NAVI_TO_COMPANY:
                //导航回家/去公司，未设置地址
                return VoiceSearchManager.getInstance().naviToHomeCompany(sessionId, poiType, poi, poiCallback);
            default:
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_SETTING_TYPE);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onLocationAsk");
        }
        MapStateManager.getInstance().openMapWhenBackground();

        final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
        final double lon = null != locInfoBean ? locInfoBean.getLongitude() : 0.0;
        final double lat = null != locInfoBean ? locInfoBean.getLatitude() : 0.0;
        if (null == locInfoBean || Double.compare(lon, 0.0) <= 0 || Double.compare(lat, 0.0) <= 0) {
            Logger.e(IVrBridgeConstant.TAG, "curLocation is empty");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_LOCATE_POS);
        } else {
            //搜索Poi详情
            if (Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "ask location lon: ", lon, ", lat: ", lat);
            }
            MapStateManager.getInstance().judgeFullScreen();
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onRoadChange: road = ", road);
        }
        CallResponse response = null;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final LocParallelInfoEntity parallelInfo = MapStateManager.getInstance().getParallelInfo();
            if (null != parallelInfo && parallelInfo.getStatus() == 0) {
                if (parallelInfo.getFlag() != 0) {
                    final int flag = parallelInfo.getFlag();
                    switch (road) {
                        case IVrBridgeConstant.ParallelOption.MAIN:
                            //切换到主路
                            if (flag == 2) {
                                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.SWITCH_MAIN_ROAD);
                                NaviPackage.getInstance().voiceChangeParallelRoad(MapType.MAIN_SCREEN_MAIN_MAP,
                                        IVrBridgeConstant.ParallelOption.MAIN);
                            } else {
                                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_MAIN_ROAD);
                                MapStateManager.getInstance().openMapWhenBackground();
                            }
                            break;
                        case IVrBridgeConstant.ParallelOption.SIDE:
                            //切换到辅路
                            if (flag == 1) {
                                NaviPackage.getInstance().voiceChangeParallelRoad(MapType.MAIN_SCREEN_MAIN_MAP,
                                        IVrBridgeConstant.ParallelOption.SIDE);
                                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.SWITCH_SUB_ROAD);
                            } else {
                                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_SUB_ROAD);
                                MapStateManager.getInstance().openMapWhenBackground();
                            }
                            break;
                        default:
                            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_SUPPORT_OTHER_ROAD_OPTION);
                    }
                } else {
                    Logger.w(IVrBridgeConstant.TAG, "当前没有主辅路可切换");
                    response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_ROAD_TO_SWITCH);
                    MapStateManager.getInstance().openMapWhenBackground();
                }
            } else {
                Logger.w(IVrBridgeConstant.TAG, "平行路切换期间，不可进行切换操作");
                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.IN_PARALLEL_ROAD);
                MapStateManager.getInstance().openMapWhenBackground();
            }
        } else {
            Logger.w(IVrBridgeConstant.TAG, "not in navigation status");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.FIRST_NAVI_PLEASE);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onBridgeChange: bridge = ", bridge);
        }
        CallResponse response = null;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final LocParallelInfoEntity parallelInfo = MapStateManager.getInstance().getParallelInfo();
            if (null != parallelInfo && parallelInfo.getStatus() == 0) {
                if (parallelInfo.getHwFlag() != 0) {
                    final int hwFlag = parallelInfo.getFlag();
                    switch (bridge) {
                        case IVrBridgeConstant.ParallelOption.ON:
                            //切换到高架上
                            if (hwFlag == 2) {
                                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.SWITCH_MAIN_BRIDGE);
                                NaviPackage.getInstance().voiceChangeParallelRoad(MapType.MAIN_SCREEN_MAIN_MAP,
                                        IVrBridgeConstant.ParallelOption.ON);
                            } else {
                                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_MAIN_BRIDGE);
                                MapStateManager.getInstance().openMapWhenBackground();
                            }
                            break;
                        case IVrBridgeConstant.ParallelOption.UNDER:
                            //切换到高架下
                            if (hwFlag == 1) {
                                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.SWITCH_SUB_BRIDGE);
                                NaviPackage.getInstance().voiceChangeParallelRoad(MapType.MAIN_SCREEN_MAIN_MAP,
                                        IVrBridgeConstant.ParallelOption.UNDER);
                            } else {
                                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_SUB_BRIDGE);
                                MapStateManager.getInstance().openMapWhenBackground();
                            }
                            break;
                        default:
                            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_SUPPORT_OTHER_BRIDGE_OPTION);
                    }
                } else {
                    Logger.w(IVrBridgeConstant.TAG, "当前没有桥上桥下可切换");
                    response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_BRIDGE_TO_SWITCH);
                    MapStateManager.getInstance().openMapWhenBackground();
                }
            } else {
                Logger.w(IVrBridgeConstant.TAG, "上下桥切换期间，不可进行切换操作");
                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.IN_PARALLEL_BRIDGE);
                MapStateManager.getInstance().openMapWhenBackground();
            }
        } else {
            Logger.w(IVrBridgeConstant.TAG, "not in navigation status");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.FIRST_NAVI_PLEAS);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onForwardAsk:");
        }
        final boolean isInNavi = Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING);
        return handleForwardAsk(isInNavi, respCallback);
    }

    /**
     * 限速查询
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onSpeedLimitAsk(final RespCallback respCallback) {
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onSpeedLimitAsk:");
        }
        final CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final int limitSpeed = MapStateManager.getInstance().getLimitSpeed();
            if (limitSpeed > 0) {
                response = CallResponse.createSuccessResponse(
                        IVrBridgeConstant.ResponseString.CURRENT_SPEED_LIMIT + limitSpeed + IVrBridgeConstant.ResponseString.CAUTION_DRIVE);
            } else {
                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.NO_LIMIT_INFO);
            }
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_NAVI_NO_INFO);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onTrafficLightAsk:");
        }
        final CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            final NaviEtaInfo etaInfo = MapStateManager.getInstance().getEtaInfo();
            if (null != etaInfo) {
                final int remainLightCount = etaInfo.routeRemainLightCount;
                response = CallResponse.createSuccessResponse(
                        IVrBridgeConstant.ResponseString.DEST_REMAIN_LIGHT + remainLightCount + IVrBridgeConstant.ResponseString.TRAFFIC_LIGHT);
            } else {
                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.NO_LIGHT_INFO);
            }
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_NAVI_NO_INFO);
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
    public CallResponse onDistanceLeftAsk(@Nullable final String start, @Nullable String arrival, final RespCallback respCallback) {
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onDistanceLeftAsk: start = ", start, ", arrival = ", arrival);
        }

        if (TextUtils.isEmpty(arrival)) {
            //当前SDK语音询问"还有多久到"start和arrival都为null，默认设置为DESTINATION.
            arrival = IVrBridgeConstant.PoiType.DESTINATION;
        }
        final CallResponse response;
        switch (arrival) {
            case IVrBridgeConstant.PoiType.DESTINATION:
            case IVrBridgeConstant.PoiType.PASS_BY:
                response = getRemainDistance(arrival, respCallback);
                break;
            case IVrBridgeConstant.DestType.HOME:
                //查询到家的ETA信息
                response = getHomeEtaInfo(start, respCallback);
                break;
            case IVrBridgeConstant.DestType.COMPANY:
                //查询到公司的ETA信息
                response = getCompanyEtaInfo(start, respCallback);
                break;
            default:
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
                    final int remainDistance = naviEtaInfo.getRemainDist();
                    final String formatDistances = VoiceConvertUtil.formatDistance(remainDistance);
                    response = CallResponse.createSuccessResponse(
                            IVrBridgeConstant.ResponseString.DEST_REMAIN_DIST + formatDistances);
                } else {
                    //途径点
                    if (null != naviEtaInfo.viaRemain && !naviEtaInfo.viaRemain.isEmpty()) {
                        final NaviEtaInfo.NaviTimeAndDist timeAndDist = naviEtaInfo.viaRemain.get(0);
                        final int viaRemainDist = timeAndDist.dist;
                        final String formatDistances = VoiceConvertUtil.formatDistance(viaRemainDist);
                        response = CallResponse.createSuccessResponse(
                                IVrBridgeConstant.ResponseString.APART_FIRST_VIA + formatDistances);
                    } else {
                        response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_INFO);
                    }
                }
            } else {
                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_INFO);
            }
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_NAVI_NO_INFO);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NET_TRASH);
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
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ADD_HOME_BEFORE_GET);
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
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ADD_COMPANY_BEFORE_GET);
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
                    if (Logger.openLog) {
                        Logger.d(IVrBridgeConstant.TAG, "homeCompanyEta: ", homeCompanyEta);
                    }
                    final CallResponse successResponse = CallResponse.createSuccessResponse(homeCompanyEta);
                    successResponse.setNeedPlayMessage(true);
                    respTts(successResponse, respCallback);
                })
                .exceptionally(throwable -> {
                    final CallResponse notSupportResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_INFO_TRY_OTHER);
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
    public CallResponse onTimeLeftAsk(@Nullable final String start, String arrival, final RespCallback respCallback) {
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onTimeLeftAsk: start = ", start, ", arrival = ", arrival);
        }
        if (TextUtils.isEmpty(arrival)) {
            arrival = IVrBridgeConstant.PoiType.DESTINATION;
        }

        final CallResponse response;
        switch (arrival) {
            case IVrBridgeConstant.PoiType.DESTINATION:
            case IVrBridgeConstant.PoiType.PASS_BY:
                response = getRemainTime(arrival, respCallback);
                break;
            case IVrBridgeConstant.DestType.HOME:
                //获取到家的ETA信息
                response = getHomeEtaInfo(start, respCallback);
                break;
            case IVrBridgeConstant.DestType.COMPANY:
                //获取到公司的ETA信息
                response = getCompanyEtaInfo(start, respCallback);
                break;
            default:
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
                    final String formatTime = VoiceConvertUtil.formatTime(naviEtaInfo.getRemainTime());
                    response = CallResponse.createSuccessResponse(
                            IVrBridgeConstant.ResponseString.DEST_REMAIN_TIME + formatTime);
                } else {
                    //第一个途径点
                    if (null != naviEtaInfo.viaRemain && !naviEtaInfo.viaRemain.isEmpty()) {
                        final NaviEtaInfo.NaviTimeAndDist timeAndDist = naviEtaInfo.viaRemain.get(0);
                        final String viaRemainTime = VoiceConvertUtil.formatTime(timeAndDist.time);
                        response = CallResponse.createSuccessResponse(
                                IVrBridgeConstant.ResponseString.APART_FIRST_POI + viaRemainTime);
                    } else {
                        response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_INFO);
                    }
                }
            } else {
                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_INFO);
            }
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_NAVI_NO_INFO);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: trafficAskBean = ", trafficAskBean.toString());
        }
        return handleTrafficConditionAsk(trafficAskBean, respCallback);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onPassByAdd: sessionId = ", sessionId, ", poi = ", poi, ", poiType = ", poiType);
        }
        if (TextUtils.isEmpty(sessionId) || TextUtils.isEmpty(poi)) {
            Logger.e(IVrBridgeConstant.TAG, "session or passBy is empty");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.PASS_BY_PARAM_EMPTY);
        }
        MapStateManager.getInstance().judgeFullScreen();
        MapStateManager.getInstance().openMapWhenBackground();

        if (!MapStateManager.getInstance().isNaviStatus()) {
            //非导航态不支持沿途搜
            Logger.w(IVrBridgeConstant.TAG, "alongSearch in no navigation");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NAVI_BEFORE_PASSBY_ROUTE);
        }
        if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            //离线算路不支持沿途搜
            Logger.w(IVrBridgeConstant.TAG, "alongSearch in offline road");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.OFFLINE_NOT_SUPPORT);
        }

        return VoiceSearchManager.getInstance().handlePassBy(sessionId, poi, poiType, poiCallback);
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onPassByDelete: sessionId = ", sessionId);
        }
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onPassByDelete: idx = ", idx);
        }

        CallResponse viaResponse;
        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        if (NaviStatus.NaviStatusType.NAVING.equals(curStatus)
                || NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus)) {
            final List<RouteParam> allPoints = RoutePackage.getInstance().getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);

            if (idx == Integer.MAX_VALUE) {
                if (allPoints.size() <= 2) {
                    viaResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_SUITABLE_VIA);
                } else {
                    for (int i = allPoints.size() - 2; i >= 1; i--) {
                        RouteParam viaParam = allPoints.get(i);
                        if (viaParam != null && !TextUtils.isEmpty(viaParam.getMPoiID())) {
                            PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                            poiInfoEntity.setPid(viaParam.getPoiID());
                            poiInfoEntity.setPoint(viaParam.getRealPos());
                            boolean isLastDelete = (i == 1);
                            RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, isLastDelete);
                        }
                    }
                    viaResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.VIA_DELETE_HINT);
                }
            }
            else if (allPoints.size() - 2 < idx) {
                viaResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_SUITABLE_VIA);
            } else {
                RouteParam viaParam = allPoints.get(idx);
                if (null == viaParam || TextUtils.isEmpty(viaParam.getMPoiID())) {
                    viaResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_SUITABLE_VIA);
                } else {
                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                    poiInfoEntity.setPid(viaParam.getPoiID());
                    poiInfoEntity.setPoint(viaParam.getRealPos());
                    RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
                    viaResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.VIA_DELETE_HINT);
                }
            }
        } else {
            viaResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.CAN_NOT_DELETE_VIA);
        }

        viaResponse.setNeedPlayMessage(true);
        return viaResponse;
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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onBroadcastSwitch: mode = ", mode);
        }

        MapStateManager.getInstance().openMapWhenBackground();
        final int muteMode = SettingPackage.getInstance().getConfigKeyMute();
        if (muteMode == 1) {
            NaviPackage.getInstance().setMute(false);
        }

        final CallResponse callResponse;
        //映射设置功能包
        final int broadcastMode;
        //用于tts播报
        final String tts;
        switch (mode) {
            case IVrBridgeConstant.VoiceBroadcastMode.BRIEF:
                broadcastMode = NaviConstant.BroadcastType.BROADCAST_CONCISE;
                tts = IVrBridgeConstant.ResponseString.BRIEF;
                break;
            case IVrBridgeConstant.VoiceBroadcastMode.DETAILED:
                broadcastMode = NaviConstant.BroadcastType.BROADCAST_DETAIL;
                tts = IVrBridgeConstant.ResponseString.DETAILED;
                break;
            case IVrBridgeConstant.VoiceBroadcastMode.MINIMALIST:
                broadcastMode = NaviConstant.BroadcastType.BROADCAST_MINIMALISM;
                tts = IVrBridgeConstant.ResponseString.MINIMALIST;
                break;
            default:
                if (Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "Go default case, no mode match ", mode, "  currently, return !");
                }
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_BROADCAST);
        }

        final int curBroadcastMode = SettingPackage.getInstance().getConfigKeyBroadcastMode();
        if (Logger.openLog) {
            Logger.i(IVrBridgeConstant.TAG, "curBroadcast: ", curBroadcastMode, ", target: ", broadcastMode);
        }
        if (broadcastMode == curBroadcastMode) {
            callResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_IN + tts);
        } else {
            final int themeMode = SettingPackage.getInstance().getConfigKeyDayNightMode();
            NaviPackage.getInstance().updateBroadcastParam(
                    broadcastMode, IVrBridgeConstant.ThemeMode.DAY == themeMode);
            callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.IN_ALREADY + tts);
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

        final boolean saveCommand = MapStateManager.getInstance().openMapWhenBackground();
        final MapMode curMapMode = MapPackage.getInstance().getCurrentMapMode(MapType.MAIN_SCREEN_MAIN_MAP);
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onMapViewSwitch mode:", mode, ", tts:", tts, ", curMode:", curMapMode.name());
        }

        final String voiceTargetMode;
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
                respTts = IVrBridgeConstant.ResponseString.NORTH_2D;
                break;
            case IVrBridgeConstant.VoiceMapMode.CAR_2D:
                targetMode = MapMode.UP_2D;
                respTts = IVrBridgeConstant.ResponseString.CAR_2D;
                break;
            case IVrBridgeConstant.VoiceMapMode.CAR_3D:
                targetMode = MapMode.UP_3D;
                respTts = tts;
                break;
            default:
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_MODE);
        }

        if (targetMode == curMapMode) {
            final CallResponse alreadyResponse = CallResponse.createSuccessResponse();
            alreadyResponse.setSubCallResult(NaviSubCallResult.NO_ACTION);
            respTts(alreadyResponse, respCallback);
        } else {
            if (saveCommand) {
                mCommandList.add(IVrBridgeConstant.VoiceCommandAction.CHANGE_VIEW);
                final SingleCommandInfo singleCommandInfo = new SingleCommandInfo();
                singleCommandInfo.setPoiName(mode);
                mCommandParamList.add(singleCommandInfo);
            } else {
                if (Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "Map view switch successfully !!! ");
                }
                MapPackage.getInstance().switchMapMode(MapType.MAIN_SCREEN_MAIN_MAP, targetMode, true);
            }
            final CallResponse callResponse = CallResponse.createSuccessResponse(
                    IVrBridgeConstant.ResponseString.IN_ALREADY + respTts);
            callResponse.setNeedPlayMessage(true);
            respTts(callResponse, respCallback);
        }

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
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "onFavoriteAdd: poi = ", poi);
        }
        if (TextUtils.isEmpty(poi)) {
            Logger.e(IVrBridgeConstant.TAG, "addFavorite poi is empty");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.EMPTY_FAVORITE);
        }
        MapStateManager.getInstance().openMapWhenBackground();

        if (IVrBridgeConstant.PoiType.DESTINATION.equals(poi)) {
            //引导目的地
            final RouteParam routeParam = RoutePackage.getInstance().getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
            if (null != routeParam) {
                final PoiInfoEntity poiInfo = getPoiInfoEntity(routeParam);
                final CallResponse destResponse;
                if (null == poiInfo || null == poiInfo.getPoint()) {
                    destResponse = CallResponse.createNotSupportResponse("获取目的地信息为空，无法收藏");
                } else {
                    if (TextUtils.isEmpty(poiInfo.getPid())) {
                        poiInfo.setPid(poiInfo.getPoint().getLon() + "_" + poiInfo.getPoint().getLat());
                    }
                    BehaviorPackage.getInstance().addFavorite(poiInfo, 0);
                    final StringBuilder builder = new StringBuilder("已收藏目的地");
                    final String name = poiInfo.getName();
                    final String address = poiInfo.getAddress();
                    if (!TextUtils.isEmpty(name)) {
                        builder.append(name);
                    } else if (!TextUtils.isEmpty(address)) {
                        builder.append(address);
                    }
                    destResponse = CallResponse.createSuccessResponse(builder.toString());
                }
                destResponse.setNeedPlayMessage(true);
                respTts(destResponse, respCallback);
            } else {
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.GET_DEST_INFO_FAIL);
            }
        } else if (IVrBridgeConstant.PoiType.CURRENT_LOCATION.equals(poi)) {
            //当前定位
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            final double lon = null != locInfoBean ? locInfoBean.getLongitude() : 0.0;
            final double lat = null != locInfoBean ? locInfoBean.getLatitude() : 0.0;
            if (null == locInfoBean || Double.compare(lon, 0.0) <= 0 || Double.compare(lat, 0.0) <= 0) {
                Logger.e(IVrBridgeConstant.TAG, "curLocation is empty");
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_GET_LOCAL_INFO);
            } else {
                //获取当前定位详情
                if (Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "addFavorite currentLocation lon: ", lon, ", lat: ", lat);
                }
                final GeoPoint geoPoint = new GeoPoint(lon, lat);
                VoiceSearchManager.getInstance().queryCurrentLocationDetail(IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE,
                        geoPoint, respCallback);
            }
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_FAVORITE_TYPE);
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
        MapStateManager.getInstance().judgeFullScreen();
        final boolean inNavigation = MapStateManager.getInstance().isNaviStatus();
        final boolean saveCommand = MapStateManager.getInstance().openMapWhenBackground();
        Logger.d(IVrBridgeConstant.TAG, "onFavoriteOpen, inNavigation:", inNavigation, ", saveCommand:", saveCommand);
        final boolean success;
        if (inNavigation) {
            success = false;
        } else {
            success = true;
            if (saveCommand) {
                mCommandList.add(IVrBridgeConstant.VoiceCommandAction.OPEN_FAVORITE);
            } else {
                openFavoritePage();
            }
        }

        if (success) {
            final CallResponse callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.OPEN_FAVORITE);
            callResponse.setNeedPlayMessage(true);
            respTts(callResponse, respCallback);
            return CallResponse.createSuccessResponse();
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_OPEN_FAVORITE_NAVI);
        }
    }

    /**
     * 真正打开收藏夹.
     */
    private void openFavoritePage() {
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.FAVORITE_PAGE);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
    }


    /**
     * 打开导航搜索记录
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onSearchListOpen(final RespCallback respCallback) {
        MapStateManager.getInstance().judgeFullScreen();
        final boolean inNavigation = MapStateManager.getInstance().isNaviStatus();
        final boolean saveCommand = MapStateManager.getInstance().openMapWhenBackground();
        Logger.d(IVrBridgeConstant.TAG, "onSearchListOpen, inNavigation:", inNavigation, ", saveCommand:", saveCommand);
        final boolean success;
        if (inNavigation) {
            success = false;
        } else {
            success = true;
            if (saveCommand) {
                mCommandList.add(IVrBridgeConstant.VoiceCommandAction.OPEN_HISTORY);
            } else {
                openHistoryPage();
            }
        }

        if (success) {
            final CallResponse callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.OPEN_HISTORY);
            callResponse.setNeedPlayMessage(true);
            respTts(callResponse, respCallback);
            return CallResponse.createSuccessResponse();
        } else {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_OPEN_HISTORY_NAVI);
        }
    }

    /**
     * 执行打开搜索导航历史记录页面.
     */
    private void openHistoryPage() {
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.SEARCH_HISTORY);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
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
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_FILTER_TYPE);
        } else {
            return CallResponse.createSuccessResponse();
        }
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
        return VoiceSearchManager.getInstance().sortPoi(sessionId, type, rule, respCallback);
    }

    @Override
    public CallResponse onPoiSort(final String s, final String s1, final String s2, final PoiCallback poiCallback) {
        return CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_VR_CTRL);
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
        return VoiceSearchManager.getInstance().handlePoiPage(sessionId, type, typeValue, respCallback);
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
        final CallResponse response;
        if (ConvertUtils.equals("CONFIRM_NO", type)) {
            if (MapStateManager.getInstance().isNaviStatus()) {
                response = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.UN_SUPPORT_IN_NAVI);
            } else {
                VoiceSearchManager.getInstance().sendClosePage();
                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.OK);
                response.setNeedPlayMessage(true);
            }
        } else {
            response = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_THIS_FUNCTION);
        }
        return response;
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
                MapStateManager.getInstance().openMapWhenBackground();
                if (null != respCallback) {
                    final CallResponse response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ALREADY_OPEN_MAP);
                    response.setNeedPlayMessage(true);
                    respCallback.onResponse(response);
                }
                break;
            case IVrBridgeConstant.MapToggleAction.CLOSE:
                closeMap(respCallback);
                break;
            default:
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_OPERATE);
        }
        return CallResponse.createSuccessResponse();
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
            final CallResponse response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ALREADY_CLOSE_MAP);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_THIS_FUNCTION);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_THIS_FUNCTION);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_TEAM);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_TEAM);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_TEAM);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_TEAM);
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
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_TEAM);
    }

    @Override
    public CallResponse onNaviToSpecifiedLocation(PoiBean poiBean, RespCallback respCallback) {
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_OPERATE);
    }

    @Override
    public CallResponse onRouteSelectInMapView(final int index, final boolean isAutoNavi) {
        Logger.d(IVrBridgeConstant.TAG, "onRouteSelectInMapView: index = ", index, "isAutoNavi = ", isAutoNavi);
        return NaviControlCommandListener.super.onRouteSelectInMapView(index, isAutoNavi);
    }

    /**
     * 语音补能规划.
     *
     * @param confirm  UPDATE_CHANGING_ROUTE
     * @param respCallback 异步执行结果.
     *
     * @return CallResponse 同步执行结果.
     */
    @Override
    public CallResponse onUpdateChangingRoute(String confirm, RespCallback respCallback) {
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "receiveChargingRoute", confirm);
        }

        final int routeChargeType = MapStateManager.getInstance().getRouteChargeType();
        if (-1 == routeChargeType) {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ROUTE_CHARGE_NO_SUPPORT);
        }

        final CallResponse chargingResponse;
        if (IVrBridgeConstant.CHANGING_ROUTE_CONFIRM.equals(confirm)) {
            if (routeChargeType == IVrBridgeConstant.RouteChargeType.SEARCH) {
                chargingResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ROUTE_CHARGE_SEARCH);
            } else {
                chargingResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ROUTE_CHARGE_REFRESH);
            }
            MapStateManager.getInstance().responseRouteCharging();
        } else {
            chargingResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.OK);
        }
        chargingResponse.setNeedPlayMessage(true);
        respCallback.onResponse(chargingResponse);
        return CallResponse.createSuccessResponse();
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
        MapStateManager.getInstance().openMapWhenBackground();

        final int muteStatus = SettingPackage.getInstance().getConfigKeyMute();
        Logger.d(IVrBridgeConstant.TAG, "onMuteToggle: open = " + open + ", current: " + muteStatus);
        final CallResponse callResponse;
        if (open) {
            if (muteStatus == 1) {
                callResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NAVI_VOICE_ALREADY_CLOSED);
            } else {
                NaviPackage.getInstance().setMute(true);
                callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.NAVI_VOICE_CLOSED);
            }
        } else {
            if (muteStatus == 0) {
                callResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NAVI_VOICE_ALREADY_OPENED);
            } else {
                NaviPackage.getInstance().setMute(false);
                callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.NAVI_VOICE_OPENED);
            }
        }
        callResponse.setNeedPlayMessage(true);
        respTts(callResponse, callback);

        return CallResponse.createSuccessResponse();
    }

    @Override
    public CallResponse onVolumeAdjust(final NaviControlParam naviControlParam, final RespCallback callback) {
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_THIS_FUNCTION);
    }

    /**
     * 通用多轮确认与否
     *
     * @param action    "CONFIRM"：确认；"CONFIRM_NO"：取消
     * @param intention 确认环节对应的意图，"ASSIGNED_ROAD"：走xx路；"NON_ASSIGNED_ROAD"：不走xx路
     * @param respCallback 异步结果回调.
     *
     * @return CallResponse.
     */
    @Override
    public CallResponse onMultipleRoundsConfirmOrNot(String action, String intention, RespCallback respCallback) {
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "multipleRound", action, intention);
        }
        final CallResponse response;
        switch (action) {
            case IVrBridgeConstant.MultipleRoundAction.CONFIRM:
                //确认
                if (null != mNewRoute) {
                    NaviPackage.getInstance().selectMainPathID(mNewRoute.getPathId());
                }
                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ROAD_ASSIGN_DONE);
                break;
            case IVrBridgeConstant.MultipleRoundAction.NO:
                //取消
                response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.ROAD_ASSIGN_NO);
                break;
            default:
                response = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_OPERATE);
                break;
        }

        if (null != respCallback) {
            response.setNeedPlayMessage(true);
            respCallback.onResponse(response);
        }

        return CallResponse.createSuccessResponse();
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

    /**
     * 保存搜索/导航参数，底图加载完成后再继续执行.
     *
     * @param sessionId   String，语音多轮一致性.
     * @param arrivalBean ArrivalBean，搜索算路参数.
     * @param poiCallback PoiCallback，语音指令执行回调.
     */
    public void saveNaviCommand(final String sessionId, final ArrivalBean arrivalBean, final PoiCallback poiCallback) {
        mCommandList.add(IVrBridgeConstant.VoiceCommandAction.ROUTE_NAVIGATION);
        final SingleCommandInfo singleCommandInfo = new SingleCommandInfo();
        singleCommandInfo.setPoiName(sessionId);
        singleCommandInfo.setArrivalBean(arrivalBean);
        singleCommandInfo.setPoiCallback(poiCallback);
        mCommandParamList.add(singleCommandInfo);
    }

    /**
     * 底图加载完成后执行之前保存的指令.
     */
    public void processNextCommand() {
        if (null == mCommandList || mCommandList.isEmpty()) {
            return;
        }

        final String nextCommand = mCommandList.remove(0);
        switch (nextCommand) {
            case IVrBridgeConstant.VoiceCommandAction.OPEN_FAVORITE:
                //打开收藏
                openFavoritePage();
                break;
            case IVrBridgeConstant.VoiceCommandAction.OPEN_HISTORY:
                //打开导航搜索历史记录
                openHistoryPage();
                break;
            case IVrBridgeConstant.VoiceCommandAction.COLLECT_COMMON:
                //onCommonPoiSet
                if (null == mCommandParamList || mCommandParamList.isEmpty()) {
                    return;
                }
                final SingleCommandInfo singleCommandInfo = mCommandParamList.remove(0);
                if (null != singleCommandInfo) {
                    processPoiSetCommand("default", singleCommandInfo.getPoiType(),
                            singleCommandInfo.getPoiName(), singleCommandInfo.getPoiCallback());
                }
                break;
            case IVrBridgeConstant.VoiceCommandAction.CHANGE_VIEW:
                //切换视角
                if (null == mCommandParamList || mCommandParamList.isEmpty()) {
                    return;
                }
                final SingleCommandInfo mapModeCommand = mCommandParamList.remove(0);
                if (null == mapModeCommand || TextUtils.isEmpty(mapModeCommand.getPoiName())) {
                    return;
                }
                final String voiceTargetMode = mapModeCommand.getPoiName();
                MapMode targetMode = null;
                switch (voiceTargetMode) {
                    case IVrBridgeConstant.VoiceMapMode.NORTH_2D:
                        targetMode = MapMode.NORTH_2D;
                        break;
                    case IVrBridgeConstant.VoiceMapMode.CAR_2D:
                        targetMode = MapMode.UP_2D;
                        break;
                    case IVrBridgeConstant.VoiceMapMode.CAR_3D:
                        targetMode = MapMode.UP_3D;
                        break;
                    default:
                        break;
                }
                if (null != targetMode) {
                    MapPackage.getInstance().switchMapMode(MapType.MAIN_SCREEN_MAIN_MAP, targetMode, true);
                }
                break;
            case IVrBridgeConstant.VoiceCommandAction.ROUTE_NAVIGATION:
                //搜索导航意图
                final SingleCommandInfo routeNaviCommand = mCommandParamList.remove(0);
                if (null != routeNaviCommand && null != routeNaviCommand.getArrivalBean()
                        && null != routeNaviCommand.getPoiCallback()) {
                    VoiceSearchManager.getInstance().handleCommonSearch(routeNaviCommand.getPoiName(),
                            routeNaviCommand.getArrivalBean(), routeNaviCommand.getPoiCallback());
                }
                break;
            default:
                break;
        }
    }


    /**
     * 处理前方如何走
     *
     * @param isNavi       是否导航
     * @param respCallBack respCallBack
     * @return CallResponse
     */
    public CallResponse handleForwardAsk(final boolean isNavi, final RespCallback respCallBack) {

        if (!isNavi) {
            Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: 非导航模式");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_NAVI_NO_INFO);
        }

        final NaviEtaInfo etaInfo = NaviPackage.getInstance().getCurrentNaviEtaInfo();
        String ttsContent = "";
        final int id = etaInfo.getCurManeuverID();
        Logger.d(IVrBridgeConstant.TAG, "转向id = " + id);
        ttsContent = switch (id) {
            case 2 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_TURN_LEFT;
            case 3 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_TURN_RIGHT;
            case 4 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_SLIGHT_LEFT;
            case 5 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_SLIGHT_RIGHT;
            case 6 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_TURN_HARD_LEFT;
            case 7 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_TURN_HARD_RIGHT;
            case 8 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_U_TURN;
            case 9 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_CONTINUE;
            case 10 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_WAY;
            case 11 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING;
            case 12 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING;
            case 13 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_SAPA;
            case 14 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_TOLLGATE;
            case 15 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_DESTINATION;
            case 16 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_TUNNEL;
            case 17 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_LEFT_RING;
            case 18 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_LEFT_RING;
            case 19 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_U_TURN_RIGHT;
            case 20 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_SPECIAL_CONTINUE;
            case 21 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING_LEFT;
            case 22 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING_RIGHT;
            case 23 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING_CONTINUE;
            case 24 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING_U_TURN;
            case 25 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_LEFT_RING_LEFT;
            case 26 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_LEFT_RING_RIGHT;
            case 27 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_LEFT_RING_CONTINUE;
            case 28 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_LEFT_RING_U_TURN;
            case 68 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING1;
            case 69 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING2;
            case 70 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING3;
            case 71 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING4;
            case 72 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING5;
            case 73 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING6;
            case 74 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING7;
            case 75 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING8;
            case 76 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING9;
            case 77 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_ENTRY_RING10;
            case 79 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING1;
            case 80 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING2;
            case 81 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING3;
            case 82 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING4;
            case 83 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING5;
            case 84 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING6;
            case 85 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING7;
            case 86 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING8;
            case 87 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING9;
            case 88 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_LEAVE_RING10;
            case 65 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_MERGE_LEFT;
            case 66 -> IVrBridgeConstant.ResponseString.MANEUVER_ICON_MERGE_RIGHT;
            default -> IVrBridgeConstant.ResponseString.UNKNOWN_ROAD_INFO;
        };

        if (!ConvertUtils.isEmpty(respCallBack)) {
            Logger.d(IVrBridgeConstant.TAG, "转向ttsContent = " + ttsContent);
            final CallResponse response = CallResponse.createSuccessResponse(ttsContent);
            response.setNeedPlayMessage(true);
            respCallBack.onResponse(response);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 处理前方路况询问
     *
     * @param isInNavi     是否导航
     * @param respCallback 结果异步回调.
     * @return CallResponse，指令回复.
     */
    public CallResponse handleForwardCondition(final boolean isInNavi, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: 转为查询前方路况");
        final CallResponse callResponse;
        if (isInNavi) {
            Logger.d(IVrBridgeConstant.TAG, "onForwardAsk: navi state");
            final RouteCurrentPathParam curPath
                    = RoutePackage.getInstance().getCurrentPathInfo(MapType.MAIN_SCREEN_MAIN_MAP);
            if (null != curPath && !curPath.isMIsOnlineRoute()) {
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.OFFLINE_CANT_SEARCH);
            }
            final String tmcStatus = NaviPackage.getInstance().getFrontTmcStatus();
            if (ConvertUtils.isEmpty(tmcStatus)) {
                callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.NO_ROAD_INFO);
            } else {
                callResponse = CallResponse.createSuccessResponse(tmcStatus);
            }
        } else {
            callResponse = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.NO_NAVI_NO_INFO);
        }
        callResponse.setNeedPlayMessage(true);
        if (null != respCallback) {
            respCallback.onResponse(callResponse);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 处理路况查询
     *
     * @param trafficAskBean trafficAskBean
     * @param respCallback   respCallback
     * @return CallResponse
     */
    public CallResponse handleTrafficConditionAsk(final TrafficAskBean trafficAskBean, final RespCallback respCallback) {
        mTrafficConditionResult = -2;//重置变量
        mIsInRoute = false;
        mHasProcessed = false;
        mTtsContentForCondition = "";
        mAllPoiParamList = null;
        mNameMap = new HashMap<>();
        if (trafficAskBean == null) {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ABNORMAL_POI);
        }
        final boolean isInNavi = Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING);
        String strPoi = trafficAskBean.getPoi();
        String strStart = trafficAskBean.getStart();
        String strArrival = trafficAskBean.getArrival();
        if (ConvertUtils.isEmpty(strArrival) && ConvertUtils.isEmpty(strPoi) && ConvertUtils.isEmpty(strStart)) {
            //trafficAskBean的成员变量全是空
            return handleForwardCondition(isInNavi, respCallback);
        }
        /*以下为有地点信息的查询*/
        if (!isInNavi) {
            Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: 非导航模式");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ONLY_SUPPORT_POI_ON_ROUTE);
        }
        mAllPoiParamList = RoutePackage.getInstance().getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (mAllPoiParamList.isEmpty()) {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_ROUTE_INFO);
        }
        mAllPoiParamList.remove(0);
        Logger.d(IVrBridgeConstant.TAG, "poiListSize -> " + mAllPoiParamList.size());
        for (RouteParam poi : mAllPoiParamList) {
            final String name = poi.getName();
            if (!ConvertUtils.isEmpty(poi.getName())) {
                mNameMap.put(poi, name.replace("(", "").replace(")", ""));
            }
            Logger.d(IVrBridgeConstant.TAG, poi.getName() + " -> " + mNameMap.get(poi));
        }

        if (ConvertUtils.equals(strPoi, IVrBridgeConstant.PoiType.DESTINATION)) {
            strPoi = mNameMap.get(mAllPoiParamList.get(mAllPoiParamList.size() - 1));
            Logger.d(IVrBridgeConstant.TAG, "POI DESTINATION -> " + strPoi);
        } else if (ConvertUtils.equals(strPoi, IVrBridgeConstant.PoiType.PASS_BY)) {
            strPoi = mNameMap.get(mAllPoiParamList.get(0));
            Logger.d(IVrBridgeConstant.TAG, "POI PASSBY -> " + strPoi);
        }


        if (ConvertUtils.equals(strStart, "我的位置")) {
            strStart = null;
            Logger.d(IVrBridgeConstant.TAG, "我的位置 -> null");
        }
        if (ConvertUtils.equals(strArrival, IVrBridgeConstant.PoiType.DESTINATION)) {
            strArrival = mNameMap.get(mAllPoiParamList.get(mAllPoiParamList.size() - 1));

            Logger.d(IVrBridgeConstant.TAG, " ARRIVAL DESTINATION -> " + strArrival);
        } else if (ConvertUtils.equals(strArrival, IVrBridgeConstant.PoiType.PASS_BY)) {
            strArrival = mNameMap.get(mAllPoiParamList.get(0));
            Logger.d(IVrBridgeConstant.TAG, " ARRIVAL PASSBY -> " + strArrival);
        }

        handlePOI(strPoi, strStart, strArrival);

        if (ConvertUtils.equals(mTrafficConditionResult, -2)) {
            handlePath(strPoi, strStart, strArrival);
        }

        Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: conditionResult = " + mTrafficConditionResult);
        mTtsContentForCondition = mapTtsContent(mTtsContentForCondition, mTrafficConditionResult);
        if (!mIsInRoute && mHasProcessed) {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ONLY_SUPPORT_POI_ON_ROUTE);
        } else {
            Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: ttsContent = " + mTtsContentForCondition);
            final CallResponse callResponse = CallResponse.createSuccessResponse(mTtsContentForCondition);
            callResponse.setNeedPlayMessage(true);
            if (null != respCallback) {
                respCallback.onResponse(callResponse);
            }
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 处理线路上的道路信息
     *
     * @param strPoi     strPoi
     * @param strStart   strStart
     * @param strArrival strArrival
     */
    private void handlePath(final String strPoi, final String strStart, final String strArrival) {
        if (!ConvertUtils.isEmpty(strPoi) && ConvertUtils.isEmpty(strStart) && ConvertUtils.isEmpty(strArrival)) {
            mHasProcessed = true;
            boolean hasEqual = false;
            final RoadName roadInfo = NaviPackage.getInstance().getAllRoadName(MapType.MAIN_SCREEN_MAIN_MAP);
            if (ConvertUtils.isEmpty(roadInfo) || ConvertUtils.isEmpty(roadInfo.getRoadNameMap())) {
                Logger.d(IVrBridgeConstant.TAG, "roadInfo error...");
                return;
            }
            final HashMap<String, Integer> roadMap = roadInfo.getRoadNameMap();
            Logger.d(IVrBridgeConstant.TAG, "PathId = " + roadInfo.getPathId());
            for (Map.Entry<String, Integer> entry : roadMap.entrySet()) {
                if (!ConvertUtils.isEmpty(entry.getKey()) && entry.getKey().contains(strPoi)) {
                    Logger.d(IVrBridgeConstant.TAG, "某道路的路况");
                    if (hasEqual) {
                        continue;
                    }
                    if (entry.getKey().equals(strPoi)) {
                        hasEqual = true;
                    }
                    mTrafficConditionResult
                            = NaviPackage.getInstance().getTmcByRoadLinkIndex(MapType.MAIN_SCREEN_MAIN_MAP, roadInfo.getPathId(), entry.getValue());
                    mIsInRoute = true;
                    mTtsContentForCondition = strPoi + "路况 ";
                }
            }
        }
    }

    /**
     * 处理线路上的地点
     *
     * @param strPoi     strPoi
     * @param strStart   strStart
     * @param strArrival strArrival
     */
    private void handlePOI(final String strPoi, final String strStart, final String strArrival) {
        if (!ConvertUtils.isEmpty(strPoi) && ConvertUtils.isEmpty(strStart) && ConvertUtils.isEmpty(strArrival)) {
            mHasProcessed = true; // 查询A地附近的路况或A道路的路况
            for (RouteParam poi : mAllPoiParamList) {
                if (!ConvertUtils.isEmpty(poi.getName()) && mNameMap.get(poi).contains(strPoi)) {
                    Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: A地附近情况");
                    mTrafficConditionResult = NaviPackage.getInstance().getTmcStatus(
                            poi.getName(), strStart, strArrival, MapType.MAIN_SCREEN_MAIN_MAP
                    );
                    mIsInRoute = true;
                    mTtsContentForCondition = strPoi + IVrBridgeConstant.ResponseString.ROAD_CONDITION;
                }
            }
        }
        if (ConvertUtils.isEmpty(strPoi) && !ConvertUtils.isEmpty(strStart) && !ConvertUtils.isEmpty(strArrival)) {
            mHasProcessed = true;// 查询A点-B点的路况
            RouteParam startPoi = null;
            RouteParam arrivalPoi = null;
            for (RouteParam poi : mAllPoiParamList) {
                if (!ConvertUtils.isEmpty(poi.getName()) && mNameMap.get(poi).contains(strStart)) {
                    Logger.d(IVrBridgeConstant.TAG, "allPoiParamList: 找到起始点");
                    startPoi = poi;
                }
                if (!ConvertUtils.isEmpty(poi.getName()) && mNameMap.get(poi).contains(strArrival)) {
                    Logger.d(IVrBridgeConstant.TAG, "allPoiParamList: 找到终点");
                    arrivalPoi = poi;
                }
            }
            if (startPoi != null && arrivalPoi != null) {
                Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: A到B的情况");
                Logger.d(IVrBridgeConstant.TAG, "startPoi name: " + startPoi.getName() + "; arrivalPoi name: " + arrivalPoi.getName());
                mTrafficConditionResult = NaviPackage.getInstance().getTmcStatus(
                        strPoi, startPoi.getName(), arrivalPoi.getName(), MapType.MAIN_SCREEN_MAIN_MAP
                );
                mIsInRoute = true;
                mTtsContentForCondition = strStart + "到" + strArrival + IVrBridgeConstant.ResponseString.ROAD_CONDITION;
            }
        }
        if (ConvertUtils.isEmpty(strPoi) && ConvertUtils.isEmpty(strStart) && !ConvertUtils.isEmpty(strArrival)) {
            mHasProcessed = true;// 查询到B的路况
            for (RouteParam poi : mAllPoiParamList) {
                if (!ConvertUtils.isEmpty(poi.getName()) && mNameMap.get(poi).contains(strArrival)) {
                    Logger.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: 当前位置到B的情况");
                    mTrafficConditionResult = NaviPackage.getInstance().getTmcStatus(
                            strPoi, strStart, poi.getName(), MapType.MAIN_SCREEN_MAIN_MAP
                    );
                    mIsInRoute = true;
                    mTtsContentForCondition = IVrBridgeConstant.ResponseString.CURRENT_POS + strArrival + IVrBridgeConstant.ResponseString.ROAD_CONDITION;
                }
            }
        }
    }

    /**
     * 映射tts内容
     *
     * @param orgTtsContent   原tts
     * @param conditionResult 路况结果
     * @return 映射后的tts
     */
    private String mapTtsContent(final String orgTtsContent, final int conditionResult) {
        return switch (conditionResult) {
            case 1 -> orgTtsContent + IVrBridgeConstant.ResponseString.SMOOTH;
            case 2 -> orgTtsContent + IVrBridgeConstant.ResponseString.LITTLE_SMOOTH;
            case 3 -> orgTtsContent + IVrBridgeConstant.ResponseString.BLOCK;
            case 4 -> orgTtsContent + IVrBridgeConstant.ResponseString.SEVER_BLOCK;
            case 5 -> orgTtsContent + IVrBridgeConstant.ResponseString.SEVER_SMOOTH;
            case -1 -> IVrBridgeConstant.ResponseString.NO_ROAD_CONDITION;
            //case 0 -> IVrBridgeConstant.ResponseString.UNKNOWN_ROAD_CONDITION;
            case 0 -> null;
            default -> IVrBridgeConstant.ResponseString.UNKNOWN_SEARCH_PARAM;
        };
    }
}