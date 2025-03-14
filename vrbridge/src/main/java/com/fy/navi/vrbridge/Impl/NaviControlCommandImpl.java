package com.fy.navi.vrbridge.Impl;


import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.baidu.oneos.protocol.bean.CallResponse;
import com.baidu.oneos.protocol.bean.TrafficAskBean;
import com.baidu.oneos.protocol.bean.param.NaviControlParam;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.callback.RespCallback;
import com.baidu.oneos.protocol.listener.NaviControlCommandListener;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
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
import com.fy.navi.vrbridge.AmapStateUtils;
import com.fy.navi.vrbridge.IVrBridgeConstant;
import com.fy.navi.vrbridge.MapStateManager;
import com.fy.navi.vrbridge.VoiceConvertUtil;
import com.fy.navi.vrbridge.bean.MapState;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;

public class NaviControlCommandImpl implements NaviControlCommandListener {

    private static final int MAP_DEFAULT_MODE = 16;
    private static final int MAP_DAY_MODE = 17;
    private static final int MAP_NIGHT_MODE = 18;

    /**
     * 放大/缩小地图
     *
     * @param zoomIn       true:放大; false:缩小
     * @param size         Integer.MAX_VALUE: 放大到最大；Integer.MIN_VALUE: 缩小到最小；1: 默认放大\缩小
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onMapSizeAdjust(boolean zoomIn, int size, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onMapSizeAdjust: zoomIn = " + zoomIn + ", size = " + size);
        CallResponse callResponse;
        float curSize = MapPackage.getInstance().getZoomLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        //TODO地图未打开，打开地图

        float levelSize = -1F;
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
                Log.d(IVrBridgeConstant.TAG, "Error zoom action !");
                callResponse = CallResponse.createNotSupportResponse("不支持的的缩放指令");
                if (respCallback != null) {
                    respCallback.onResponse(callResponse);
                }
                return CallResponse.createSuccessResponse();
        }

        if (zoomIn) {
            if (curSize == AutoMapConstant.MAP_ZOOM_LEVEL_MAX) {
                callResponse = CallResponse.createSuccessResponse("当前已为最大地图");
            } else {
                if (levelSize == AutoMapConstant.MAP_ZOOM_LEVEL_MAX) {
                    //直接放到最大
                    Log.d(IVrBridgeConstant.TAG, "Set map level max");
                    MapPackage.getInstance().setZoomLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP, levelSize);
                    callResponse = CallResponse.createSuccessResponse("已放大地图到最大");
                } else {
                    //默认flag提升
                    Log.d(IVrBridgeConstant.TAG, "Amplify map level");
                    MapPackage.getInstance().amplifyLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP);
                    callResponse = CallResponse.createSuccessResponse("地图已放大");
                }
            }
        } else {
            if (curSize == AutoMapConstant.MAP_ZOOM_LEVEL_MIN) {
                callResponse = CallResponse.createSuccessResponse("当前已为最小地图");
            } else {
                if (levelSize == AutoMapConstant.MAP_ZOOM_LEVEL_MIN) {
                    Log.d(IVrBridgeConstant.TAG, "Set map level min");
                    MapPackage.getInstance().setZoomLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP, levelSize);
                    callResponse = CallResponse.createSuccessResponse("已缩小地图到最小");
                } else {
                    Log.d(IVrBridgeConstant.TAG, "Reduce map level");
                    MapPackage.getInstance().reduceLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP);
                    callResponse = CallResponse.createSuccessResponse("地图已缩小");
                }
            }
        }
        Log.d(IVrBridgeConstant.TAG, "map state : " + MapState.getInstance().getCurrZoomLevel());
        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }

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
    public CallResponse onRouteOverviewToggle(boolean open, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onRouteOverviewToggle: open = " + open);
        if (open) {
            //查看全程
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createSuccessResponse("已展示路线全览"));
            }
            NaviPackage.getInstance().voiceRouteOverview(MapTypeId.MAIN_SCREEN_MAIN_MAP, true);
        } else {
            //退出全程
            if (MapStateManager.getInstance().isNaviStatus()) {
                if (null != respCallback) {
                    respCallback.onResponse(CallResponse.createSuccessResponse("已退出路线全览"));
                }
                NaviPackage.getInstance().voiceRouteOverview(MapTypeId.MAIN_SCREEN_MAIN_MAP, false);
            } else {
                //todo 非导航态，HMI如果处于后台，则切换到前台
                if (!NaviPackage.getInstance().getIsAppInForeground()) {

                }
            }
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 打开/关闭经典导航
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    @Deprecated
    public CallResponse onClassicNaviToggle(boolean open, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onClassicNaviToggle: open = " + open);
        return CallResponse.createNotSupportResponse("暂不支持切换经典导航");
    }

    /**
     * 打开/关闭轻导航
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    @Deprecated
    public CallResponse onFamiliarNaviToggle(boolean open, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onFamiliarNaviToggle: open = " + open);
        return null;
    }

    /**
     * 打开/关闭AR导航
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    @Deprecated
    public CallResponse onArNaviToggle(boolean open, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onArNaviToggle: open = " + open);
        return CallResponse.createNotSupportResponse("暂不支持切换AR导航");
    }

    /**
     * 切换导航模式
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    @Deprecated
    public CallResponse onNaviModeSwitch(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onFamiliarNaviToggle:");
        return CallResponse.createNotSupportResponse("暂不支持切换导航模式");
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
    public CallResponse onNaviActionChange(String action, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onNaviActionChange: action = " + action);
        CallResponse callResponse;

        String curNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        switch (action) {
            case "START_NAVIGATION":
                if (Objects.equals(curNaviStatus, NaviStatus.NaviStatusType.NAVING)) {
                    //TODO 后台的话切到前台
                    //if (AppContext.mApplication.) {
                    //    Log.d(IVrBridgeConstant.TAG, "App background to foreground !");
                    //}
                    Log.d(IVrBridgeConstant.TAG, "Already in navigation state !");
                    callResponse = CallResponse.createSuccessResponse("当前已在导航中");
                } else if (Objects.equals(curNaviStatus, NaviStatus.NaviStatusType.SELECT_ROUTE)) {
                    Log.d(IVrBridgeConstant.TAG, "Selecting route page, start navigation !");
                    Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.START_NAVIGATION);
                    MapPackage.getInstance().voiceOpenSearchPage(MapTypeId.MAIN_SCREEN_MAIN_MAP, bundle);
                    callResponse = CallResponse.createSuccessResponse("开始出发");
                } else {
                    Log.d(IVrBridgeConstant.TAG, "Haven't select route, keep listen !");
                    callResponse = CallResponse.createSuccessResponse("请问你要去哪里呢");
                }
                break;
            case "END_NAVIGATION":
                if (Objects.equals(curNaviStatus, NaviStatus.NaviStatusType.NAVING)) {
                    Log.d(IVrBridgeConstant.TAG, "Stop navigation successfully !");
                    NaviPackage.getInstance().stopNavigation();
                    callResponse = CallResponse.createSuccessResponse("导航结束");
                } else {
                    Log.d(IVrBridgeConstant.TAG, "Already end navigation !");
                    callResponse = CallResponse.createSuccessResponse("当前不在导航状态");
                }
                break;
            case "CONTINUE_NAVIGATION":
                Log.d(IVrBridgeConstant.TAG, "Continue navigation !");
                //TODO 区分触碰态、全览态与沉浸态，需要与界面互动
                int sceneState = NaviPackage.getInstance().getCurrentImmersiveStatus();
                if (sceneState == 1) {
                    //沉浸态
                } else if (sceneState == 2) {
                    //触碰态
                } else if (MapPackage.getInstance().getIsEnterPreview(MapTypeId.MAIN_SCREEN_MAIN_MAP)) {
                    //全览态
                }
                callResponse = CallResponse.createSuccessResponse("已恢复导航");
                break;
            default:
                Log.d(IVrBridgeConstant.TAG, "Error navi action !");
                callResponse = CallResponse.createNotSupportResponse("不支持的的导航指令");
        }
        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 打开/关闭巡航模式
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onEDogModeToggle(boolean open, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onEDogModeToggle: open = " + open);
        CallResponse callResponse;
        //TODO地图未打开，打开地图

        //TODO 判断是否已经打开/关闭此功能

        if (open) {
            boolean successOpen = CruisePackage.getInstance().startCruise();
            Log.d(IVrBridgeConstant.TAG, "onEDogModeToggle open " + (successOpen ? "succeed" : "failed") + " !");
            if (successOpen) {
                callResponse = CallResponse.createSuccessResponse("自动进入巡航已开启，会在满足条件时自动进入巡航模式");
            } else {
                callResponse = CallResponse.createFailResponse("自动进入巡航功能开启失败");
            }
        } else {
            boolean successClose = CruisePackage.getInstance().stopCruise();
            Log.d(IVrBridgeConstant.TAG, "onEDogModeToggle close " + (successClose ? "succeed" : "failed") + " !");
            if (successClose) {
                callResponse = CallResponse.createSuccessResponse("自动进入巡航功能已关闭");
            } else {
                callResponse = CallResponse.createFailResponse("自动进入巡航功能关闭失败");
            }
        }

        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 打开/关闭路况
     *
     * @param open         true:打开；false:关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    @Deprecated
    public CallResponse onTrafficModeToggle(boolean open, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onTrafficModeToggle: open = " + open);
        CallResponse callResponse;

        //TODO 地图未打开，打开地图

        if (open == SettingPackage.getInstance().getConfigKeyRoadEvent()) {
            //TODO如果在后台则切到前台
            callResponse = CallResponse.createSuccessResponse("路况已经" + (open ? "打开" : "关闭"));
        } else {
            SettingPackage.getInstance().setConfigKeyRoadEvent(open);
            callResponse = CallResponse.createSuccessResponse(open ? "打开" : "关闭" + "路况成功 ！");
        }

        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 走某道路
     *
     * @param road         道路名称
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    @Deprecated
    public CallResponse onRoadAssigned(String road, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onRoadAssigned: road = " + road);
        //TODO
        return null;
    }

    /**
     * 不走某道路
     *
     * @param road         道路名称
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    @Deprecated
    public CallResponse onRoadNonAssigned(String road, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onRoadNonAssigned: road = " + road);
        //TODO
        //RouteLineSegmentInfo
        RouteAvoidInfo info = new RouteAvoidInfo();
        RoutePackage.getInstance().setAvoidRoad(info);
        //RoutePackage.getInstance().setAvoidRoad(routeAvoidInfo);
        //RoutePackage.getInstance().requestRoute(mMapTypeId, null, -1, false, RouteWayID.ROUTE_WAY_AVOID);
        return null;
    }

    /**
     * 刷新路线
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    @Deprecated
    public CallResponse onRouteRefresh(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onRouteRefresh:");

        CallResponse callResponse;
        List<RouteParam> allPoiParamList = RoutePackage.getInstance().getAllPoiParamList(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            //TODO Timer
            RouteParam routeParam = allPoiParamList.get(allPoiParamList.size() - 1);
            RoutePackage.getInstance().requestRoute(MapTypeId.MAIN_SCREEN_MAIN_MAP, NaviDataFormatHelper.getPoiInfoEntity(routeParam), routeParam.getPoiType(), true, RouteWayID.ROUTE_WAY_REFRESH);
            callResponse = CallResponse.createSuccessResponse("已为你更新路线");
        } else {
            callResponse = CallResponse.createFailResponse("刷新路线失败");
        }

        if (respCallback != null) {
            respCallback.onResponse(callResponse);
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
    public CallResponse onRouteSwitch(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onRouteSwitch:");
        CallResponse callResponse;
        RoutePreferenceID curRouteId = SettingPackage.getInstance().getRoutePreference();
        switch (curRouteId) {
            case PREFERENCE_RECOMMEND:
                curRouteId = RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
                break;
            case PREFERENCE_NOTHIGHWAY:
                curRouteId = RoutePreferenceID.PREFERENCE_LESSCHARGE;
                break;
            case PREFERENCE_LESSCHARGE:
                curRouteId = RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
                break;
            case PREFERENCE_AVOIDCONGESTION:
                curRouteId = RoutePreferenceID.PREFERENCE_FASTESTSPEED;
                break;
            case PREFERENCE_FASTESTSPEED:
                curRouteId = RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
                break;
            case PREFERENCE_FIRSTHIGHWAY:
                curRouteId = RoutePreferenceID.PREFERENCE_RECOMMEND;
                break;
            default:
                Log.d(IVrBridgeConstant.TAG, "Go default case, no preference match currently, return !");
                callResponse = CallResponse.createNotSupportResponse("切换路线失败，请重试 ！");
                if (respCallback != null) {
                    respCallback.onResponse(callResponse);
                }
                return CallResponse.createSuccessResponse();
        }

        SettingPackage.getInstance().setRoutePreference(curRouteId);
        callResponse = CallResponse.createSuccessResponse("已切换路线偏好");

        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }
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
    public CallResponse onRouteChoose(String type, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onRoadChoose: road = " + type);
        CallResponse callResponse;

        //用于存入settingPackage
        RoutePreferenceID routeId;
        //用于返回tts
        String tts = "";
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
            case "CHOOSE_AUTOP_ROUTE":
            default:
                Log.d(IVrBridgeConstant.TAG, "Go default case, no preference match currently, return !");
                callResponse = CallResponse.createFailResponse("更换偏好失败，请重试！");
                if (respCallback != null) {
                    respCallback.onResponse(callResponse);
                }
                return CallResponse.createSuccessResponse();
        }

        if (routeId == SettingPackage.getInstance().getRoutePreference()) {
            //TODO 如果是后台，切到前台
            callResponse = CallResponse.createSuccessResponse("当前已是" + tts + "的路线");
        } else {
            SettingPackage.getInstance().setRoutePreference(routeId);
            callResponse = CallResponse.createSuccessResponse("已使用" + tts + "方案");
        }
        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 选择路线
     *
     * @param idx          路线索引值
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onRouteChoose(int idx, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "Route Choose by index : road = " + idx);
        MapState mapState = new MapState.Builder().build();

        int routeIndex = idx - 1;
        if (routeIndex > mapState.getPathCount() || routeIndex < 0) {
            CallResponse callResponse = CallResponse.createFailResponse("选择超出范围");
            if (respCallback != null) {
                respCallback.onResponse(callResponse);
            }
            return CallResponse.createSuccessResponse();
        }

        RoutePackage.getInstance().selectRoute(MapTypeId.MAIN_SCREEN_MAIN_MAP, routeIndex);

        CallResponse callResponse = CallResponse.createSuccessResponse("已为您选择第" + idx + "条路线");
        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 设置常用地址——家和公司
     *
     * @param sessionId   用来确保本轮数据的一致性
     * @param poiType     HOME：家；COMPANY：公司
     * @param poi         地址，CURRENT_LOCATION：当前位置
     * @param poiCallback poiCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onCommonPoiSet(String sessionId, String poiType, @NonNull String poi, PoiCallback poiCallback) {
        Log.d(IVrBridgeConstant.TAG, "onRoadNonAssigned: road = " + sessionId + ", poiType = " + poiType + ", poi = " + poi);
        VoiceSearchManager.getInstance().setHomeCompany(sessionId, poiType, poi, poiCallback);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 位置查询
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onLocationAsk(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onLocationAsk:");
        //todo HMI未打开则打开

        LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
        if (null == locInfoBean || Double.compare(locInfoBean.getLongitude(), 0.0) <= 0
                || Double.compare(locInfoBean.getLatitude(), 0.0) <= 0) {
            Log.e(IVrBridgeConstant.TAG, "curLocation is empty");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("不好意思，我定位不到你在哪里"));
            }
        } else {
            //搜索Poi详情
            GeoPoint geoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
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
    public CallResponse onRoadChange(String road, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onRoadChange: road = " + road);
        CallResponse response = null;
        if (MapStateManager.getInstance().isNaviStatus()) {
            LocParallelInfoEntity parallelInfo = MapStateManager.getInstance().getParallelInfo();
            if (null != parallelInfo && parallelInfo.getStatus() == 1 && parallelInfo.getFlag() != 0) {
                int flag = parallelInfo.getFlag();
                switch (road) {
                    case IVrBridgeConstant.ParallelOption.MAIN:
                        //切换到主路
                        if (flag == 2) {
                            response = CallResponse.createSuccessResponse("已切换到主路");
                            NaviPackage.getInstance().voiceChangeParallelRoad(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                                    IVrBridgeConstant.ParallelOption.MAIN);
                        } else {
                            response = CallResponse.createNotSupportResponse("当前已在主路");
                            //todo 地图在后台，切换到前台
                        }
                        break;
                    case IVrBridgeConstant.ParallelOption.SIDE:
                        //切换到辅路
                        if (flag == 1) {
                            response = CallResponse.createSuccessResponse("已切换到辅路");
                            NaviPackage.getInstance().voiceChangeParallelRoad(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                                    IVrBridgeConstant.ParallelOption.SIDE);
                        } else {
                            response = CallResponse.createNotSupportResponse("当前已在辅路");
                            //todo 地图在后台，切换到前台
                        }
                        break;
                    default:
                        break;
                }
            } else {
                Log.w(IVrBridgeConstant.TAG, "not int parallel status");
                response = CallResponse.createNotSupportResponse("当前没有主辅路可切换");

            }
        } else {
            Log.w(IVrBridgeConstant.TAG, "not in navigation status");
            response = CallResponse.createFailResponse("先发起导航才能切换主辅路");
        }
        if (null != respCallback && null != response) {
            respCallback.onResponse(response);
        }
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
    public CallResponse onBridgeChange(String bridge, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onBridgeChange: bridge = " + bridge);
        CallResponse response = null;
        if (MapStateManager.getInstance().isNaviStatus()) {
            LocParallelInfoEntity parallelInfo = MapStateManager.getInstance().getParallelInfo();
            if (null != parallelInfo && parallelInfo.getStatus() == 1 && parallelInfo.getHwFlag() != 0) {
                int hwFlag = parallelInfo.getFlag();
                switch (bridge) {
                    case IVrBridgeConstant.ParallelOption.ON:
                        //切换到高架上
                        if (hwFlag == 2) {
                            response = CallResponse.createSuccessResponse("已切换到桥上");
                            NaviPackage.getInstance().voiceChangeParallelRoad(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                                    IVrBridgeConstant.ParallelOption.ON);
                        } else {
                            response = CallResponse.createNotSupportResponse("当前已在桥上");
                            //todo 地图在后台，切换到前台
                        }
                        break;
                    case IVrBridgeConstant.ParallelOption.UNDER:
                        //切换到高架下
                        if (hwFlag == 1) {
                            response = CallResponse.createSuccessResponse("已切换到桥下");
                            NaviPackage.getInstance().voiceChangeParallelRoad(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                                    IVrBridgeConstant.ParallelOption.UNDER);
                        } else {
                            response = CallResponse.createNotSupportResponse("当前已在桥下");
                            //todo 地图在后台，切换到前台
                        }
                        break;
                    default:
                        break;
                }
            } else {
                Log.w(IVrBridgeConstant.TAG, "not int parallel status");
                response = CallResponse.createNotSupportResponse("当前没有桥上下可切换");
            }
        } else {
            Log.w(IVrBridgeConstant.TAG, "not in navigation status");
            response = CallResponse.createFailResponse("先发起导航才能切换桥上下");
        }

        if (null != respCallback && null != response) {
            respCallback.onResponse(response);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 前方如何行驶.
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onForwardAsk(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onForwardAsk:");
        return null;
    }

    /**
     * 限速查询
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onSpeedLimitAsk(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onSpeedLimitAsk:");
        CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            int limitSpeed = MapStateManager.getInstance().getLimitSpeed();
            if (limitSpeed > 0) {
                response = CallResponse.createSuccessResponse("当前道路限速" + limitSpeed + "km/h，请消息驾驶");
            } else {
                response = CallResponse.createSuccessResponse("前方暂无限速信息，不过也不要开的太快哦");
            }
        } else {
            response = CallResponse.createNotSupportResponse("当前不在导航状态，没有行程相关信息哦");
        }
        if (null != respCallback) {
            respCallback.onResponse(response);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 红绿灯查询
     *
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onTrafficLightAsk(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onTrafficLightAsk:");
        CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            NaviEtaInfo etaInfo = MapStateManager.getInstance().getEtaInfo();
            if (null != etaInfo) {
                int remainLightCount = etaInfo.routeRemainLightCount;
                response = CallResponse.createSuccessResponse("离目的地剩余" + remainLightCount + "个红绿灯");
            } else {
                response = CallResponse.createFailResponse("暂无红绿灯信息，请稍后再试");
            }
        } else {
            response = CallResponse.createNotSupportResponse("当前不在导航状态，没有行程相关信息哦");
        }
        if (null != respCallback) {
            respCallback.onResponse(response);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 剩余距离查询
     *
     * @param start        路线起点，可以为null
     * @param arrival      路线 终点，可以为null
     *                     PASSBY:途经点
     *                     DESTINATION：目的地/终点
     * @param respCallback 异步接收执行结果
     * @return CallResponse
     */
    @Override
    public CallResponse onDistanceLeftAsk(@Nullable String start, @Nullable String arrival, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onDistanceLeftAsk: start = " + start + ", arrival = " + arrival);
        if (TextUtils.isEmpty(arrival)) {
            if (null != respCallback) {
                CallResponse response = CallResponse.createFailResponse("不支持的目的地类型");
                respCallback.onResponse(response);
            }
            return CallResponse.createSuccessResponse();
        }

        switch (arrival) {
            case IVrBridgeConstant.PoiType.DESTINATION:
            case IVrBridgeConstant.PoiType.PASS_BY:
                getRemainDistance(arrival, respCallback);
                break;
            case IVrBridgeConstant.DestType.HOME:
                getHomeEtaInfo(start, respCallback);
                //查询到家的ETA信息
                break;
            case IVrBridgeConstant.DestType.COMPANY:
                getCompanyEtaInfo(start, respCallback);
                //查询到公司的ETA信息
                break;
            default:
                if (null != respCallback) {
                    CallResponse response = CallResponse.createFailResponse("不支持的目的地类型");
                    respCallback.onResponse(response);
                }
                break;
        }

        return CallResponse.createSuccessResponse();
    }

    //获取目的地或途径点的剩余距离
    private void getRemainDistance(String arrival, RespCallback respCallback) {
        CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            NaviEtaInfo naviEtaInfo = MapStateManager.getInstance().getEtaInfo();
            if (null != naviEtaInfo) {
                if (IVrBridgeConstant.PoiType.DESTINATION.equals(arrival)) {
                    //目的地
                    int remainDistance = naviEtaInfo.allDist;
                    String formatDistances = VoiceConvertUtil.formatDistance(remainDistance);
                    response = CallResponse.createSuccessResponse("距目的地还有" + formatDistances);
                } else {
                    //途径点
                    if (null != naviEtaInfo.viaRemain && !naviEtaInfo.viaRemain.isEmpty()) {
                        NaviEtaInfo.NaviTimeAndDist timeAndDist = naviEtaInfo.viaRemain.get(0);
                        int viaRemainDist = timeAndDist.dist;
                        String formatDistances = VoiceConvertUtil.formatDistance(viaRemainDist);
                        response = CallResponse.createSuccessResponse("距第一个途径点还有" + formatDistances);
                    } else {
                        response = CallResponse.createFailResponse("没有查询到相关信息，请稍后重试");
                    }
                }
            } else {
                response = CallResponse.createFailResponse("没有查询到相关新，请稍后重试");
            }
        } else {
            response = CallResponse.createNotSupportResponse("当前不在导航状态，没有行程相关信息哦");
        }
        if (null != respCallback) {
            respCallback.onResponse(response);
        }
    }

    private void getHomeEtaInfo(String start, RespCallback respCallback) {
        PoiInfoEntity homeInfo = BehaviorPackage.getInstance().getFavoriteHomeData(1);
        if (null == homeInfo) {
            Log.e(IVrBridgeConstant.TAG, "have not save homeInfo");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("未找到家的地址，先去添加吧"));
            }
            return;
        }

        if (TextUtils.isEmpty(start)) {
            //以当前位置为起点
            LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null != locInfoBean) {
                GeoPoint startPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                GeoPoint endPoint = homeInfo.getPoint();
                StringBuilder builder = new StringBuilder("到家有");
                processHomeCompanyEta(startPoint, endPoint, builder, respCallback);
            }
        } else {
            //指定地点回家的距离和时间
            VoiceSearchManager.getInstance().searchPoiInfo(IVrBridgeConstant.VoiceSearchType.TIME_AND_DIST,
                    IVrBridgeConstant.DestType.HOME, start, respCallback);
        }
    }

    private void getCompanyEtaInfo(String start, RespCallback respCallback) {
        PoiInfoEntity companyInfo = BehaviorPackage.getInstance().getFavoriteHomeData(2);
        if (null == companyInfo) {
            Log.e(IVrBridgeConstant.TAG, "have not save homeInfo");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("未找到家的地址，先去添加吧"));
            }
            return;
        }

        if (TextUtils.isEmpty(start)) {
            //以当前位置为起点
            LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null != locInfoBean) {
                GeoPoint startPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                GeoPoint endPoint = companyInfo.getPoint();
                StringBuilder builder = new StringBuilder("到公司有");
                processHomeCompanyEta(startPoint, endPoint, builder, respCallback);
            }
        } else {
            //指定地点去公司的距离和时间
            VoiceSearchManager.getInstance().searchPoiInfo(IVrBridgeConstant.VoiceSearchType.TIME_AND_DIST,
                    IVrBridgeConstant.DestType.COMPANY, start, respCallback);
        }
    }

    private void processHomeCompanyEta(GeoPoint start, GeoPoint end, StringBuilder builder, RespCallback respCallback) {
        RoutePackage.getInstance().getTravelTimeFuture(start, end)
                .thenAccept(new Consumer<Pair<String, String>>() {
                    @Override
                    public void accept(Pair<String, String> pair) {
                        String distance = pair.first;
                        String time = pair.second;
                        builder.append(distance).append("，大约需要").append(time);
                        String homeCompanyEta = builder.toString();
                        Log.d(IVrBridgeConstant.TAG, "homeCompanyEta: " + homeCompanyEta);
                        if (null != respCallback) {
                            respCallback.onResponse(CallResponse.createSuccessResponse(homeCompanyEta));
                        }
                    }
                })
                .exceptionally(new Function<Throwable, Void>() {
                    @Override
                    public Void apply(Throwable throwable) {
                        if (null != respCallback) {
                            respCallback.onResponse(CallResponse.createFailResponse("没有查询到相关信息，试试别的吧"));
                        }
                        return null;
                    }
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
    public CallResponse onTimeLeftAsk(@Nullable String start, @Nullable String arrival, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onTimeLeftAsk: start = " + start + ", arrival = " + arrival);
        if (TextUtils.isEmpty(arrival)) {
            if (null != respCallback) {
                CallResponse response = CallResponse.createFailResponse("不支持的目的地类型");
                respCallback.onResponse(response);
            }
            return CallResponse.createSuccessResponse();
        }

        switch (arrival) {
            case IVrBridgeConstant.PoiType.DESTINATION:
            case IVrBridgeConstant.PoiType.PASS_BY:
                getRemainTime(arrival, respCallback);
                break;
            case IVrBridgeConstant.DestType.HOME:
                //获取到家的ETA信息
                getHomeEtaInfo(start, respCallback);
                break;
            case IVrBridgeConstant.DestType.COMPANY:
                getCompanyEtaInfo(start, respCallback);
                //获取到公司的ETA信息
                break;
            default:
                if (null != respCallback) {
                    CallResponse response = CallResponse.createFailResponse("不支持的目的地类型");
                    respCallback.onResponse(response);
                }
                break;
        }

        return CallResponse.createSuccessResponse();
    }

    //查询到目的地或途径点的剩余时间
    private void getRemainTime(String arrival, RespCallback respCallback) {
        CallResponse response;
        if (MapStateManager.getInstance().isNaviStatus()) {
            NaviEtaInfo naviEtaInfo = MapStateManager.getInstance().getEtaInfo();
            if (null != naviEtaInfo) {
                if (IVrBridgeConstant.PoiType.DESTINATION.equals(arrival)) {
                    //目的地
                    String formatTime = VoiceConvertUtil.formatTime(naviEtaInfo.allTime);
                    response = CallResponse.createSuccessResponse("到目的地大约需要" + formatTime);
                } else {
                    //第一个途径点
                    if (null != naviEtaInfo.viaRemain && !naviEtaInfo.viaRemain.isEmpty()) {
                        NaviEtaInfo.NaviTimeAndDist timeAndDist = naviEtaInfo.viaRemain.get(0);
                        String viaRemainTime = VoiceConvertUtil.formatTime(timeAndDist.time);
                        response = CallResponse.createSuccessResponse("距第一个途径点大约需要" + viaRemainTime);
                    } else {
                        response = CallResponse.createFailResponse("没有查询到相关新，请稍后重试");
                    }
                }
            } else {
                response = CallResponse.createFailResponse("没有查询到相关新，请稍后重试");
            }
        } else {
            response = CallResponse.createNotSupportResponse("当前不在导航状态，没有行程相关信息哦");
        }
        if (null != respCallback) {
            respCallback.onResponse(response);
        }
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
    public CallResponse onTrafficConditionAsk(TrafficAskBean trafficAskBean, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: trafficAskBean = " + trafficAskBean);
        if (trafficAskBean == null) {
            return CallResponse.createFailResponse("");
        }
        List<RouteParam> allPoiParamList = RoutePackage.getInstance().getAllPoiParamList(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        if (allPoiParamList == null) {
            return CallResponse.createFailResponse("");
        }
        // 查询A地附近的路况或A道路的路况
        if (!ConvertUtils.isEmpty(trafficAskBean.getPoi())) {
            for (int i = 0; i < allPoiParamList.size(); i++) {
                RouteParam routeParam = allPoiParamList.get(i);
                if (trafficAskBean.getPoi().equals(routeParam.getName())) {
                    // TODO 路况拥堵请求
                    // 成功 tts播报
                    // 失败 暂未查询到相应路况信息，请稍后再试
                    respCallback.onResponse(CallResponse.createFailResponse("暂未查询到相应路况信息，请稍后再试"));
                    return CallResponse.createSuccessResponse();
                }
            }
            Log.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: the poi is not on the route");
            return CallResponse.createFailResponse("仅支持导航路线上的路况信息查询哦");
        }
        // 查询A点-B点的路况
        if (!ConvertUtils.isEmpty(trafficAskBean.getStart()) && !ConvertUtils.isEmpty(trafficAskBean.getArrival())) {
            HashMap<String, RouteParam> poiMap = new HashMap<>();
            for (RouteParam poi : allPoiParamList) {
                poiMap.put(poi.getName(), poi);
            }
            RouteParam startPoi = poiMap.get(trafficAskBean.getStart());
            RouteParam arrivalPoi = poiMap.get(trafficAskBean.getArrival());
            if (startPoi != null && arrivalPoi != null) {
                // TODO 路况拥堵请求
                // 成功 tts播报
                // 失败 暂未查询到相应路况信息，请稍后再试
                respCallback.onResponse(CallResponse.createFailResponse("暂未查询到相应路况信息，请稍后再试"));
                return CallResponse.createSuccessResponse();
            } else {
                Log.d(IVrBridgeConstant.TAG, "onTrafficConditionAsk: a or b is not on the route");
                return CallResponse.createFailResponse("仅支持导航路线上的路况信息查询哦");
            }
        }
        // 查询到B的路况
        if (ConvertUtils.isEmpty(trafficAskBean.getStart()) && !ConvertUtils.isEmpty(trafficAskBean.getArrival())) {
            // TODO 路况拥堵请求
        }
        // 查询前方路况
        if (ConvertUtils.isEmpty(trafficAskBean.getPoi()) && ConvertUtils.isEmpty(trafficAskBean.getStart()) && ConvertUtils.isEmpty(trafficAskBean.getArrival())) {
            // TODO 路况拥堵请求
        }
        return CallResponse.createFailResponse("仅支持导航路线上的路况信息查询哦");
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
    public CallResponse onPassbyAdd(String sessionId, String poi, String poiType, PoiCallback poiCallback) {
        Log.d(IVrBridgeConstant.TAG, "onPassbyAdd: sessionId = " + sessionId + ", poi = " + poi + ", poiType = " + poiType);
        return null;
    }

    /**
     * 删除途径点，存在多个途径点情况，需要知道途径点数据
     *
     * @param sessionId   用来确保本轮数据的一致性
     * @param poiCallback 返回多个途径点数据
     * @return CallResponse
     */
    @Override
    public CallResponse onPassbyDelete(String sessionId, PoiCallback poiCallback) {
        Log.d(IVrBridgeConstant.TAG, "onPassbyDelete: sessionId = " + sessionId);
        return null;
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
    public CallResponse onPassbyDelete(int idx, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onPassbyDelete: idx = " + idx);
        return null;
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
    public CallResponse onBroadcastSwitch(String mode, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onBroadcastSwitch: mode = " + mode);
        CallResponse callResponse;
        //TODO 地图未打开，打开地图

        //映射设置功能包
        int broadcastMode = -1;
        //用于tts播报
        String tts = "";
        switch (mode) {
            case "BRIEF":
                broadcastMode = 1;
                tts = "简洁播报模式";
                break;
            case "DETAILED":
                broadcastMode = 2;
                tts = "详细播报模式";
                break;
            case "BEEP":
                //settingPackage内 3对应极简播报
                broadcastMode = 3;
                tts = "提示音模式";
                break;
            default:
                //不播报与极简模式暂无映射
                Log.d(IVrBridgeConstant.TAG, "Go default case, no mode match " + mode + "  currently, return !");
                callResponse = CallResponse.createFailResponse("切换失败，请重试");
                if (respCallback != null) {
                    respCallback.onResponse(callResponse);
                }
        }

        if (broadcastMode == SettingPackage.getInstance().getConfigKeyBroadcastMode()) {
            callResponse = CallResponse.createSuccessResponse("当前已为" + tts);
        } else {
            Log.d(IVrBridgeConstant.TAG, "Broadcast mode switch successfully ! ");
            NaviPackage.getInstance().updateBroadcastParam(
                    broadcastMode,
                    SettingPackage.getInstance().getConfigKeyDayNightMode() != MAP_NIGHT_MODE
            );
            callResponse = CallResponse.createSuccessResponse("已切换为" + tts);
        }

        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 切换视图
     *
     * @param mode         3D：车头朝上/车头向上/跟随车头/跟随/3D;
     *                     2D：正北/正北朝上/正北向上/2D;
     *                     2D_FOLLOW_LOGO：2D车头朝上;
     *                     DEFAULT：根据当前模式切换
     * @param tts          xx模式
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onMapViewSwitch(String mode, String tts, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onMapViewSwitch: mode = " + mode + ", tts = " + tts);
        CallResponse callResponse;
        //TODO 应用关闭的情况下打开

        MapMode mapMode;
        int mapModeSetting = -1;
        String respTts = "";
        switch (mode) {
            case "3D":
                mapMode = MapMode.UP_3D;
                mapModeSetting = 2;
                respTts = "3D模式";
                break;
            case "2D":
                mapMode = MapMode.NORTH_2D;
                mapModeSetting = 1;
                respTts = "2D模式";
                break;
            case "2D_FOLLOW_LOGO":
                mapMode = MapMode.UP_2D;
                mapModeSetting = 0;
                respTts = "2D车头朝上模式";
                break;
            default:
                //DEFAULT暂时不知如何得到
                Log.d(IVrBridgeConstant.TAG, "Go default case, no mode match currently, return !");
                callResponse = CallResponse.createFailResponse("Switch failed, please try again !");
                if (respCallback != null) {
                    respCallback.onResponse(callResponse);
                }
                return CallResponse.createSuccessResponse();
        }

        if (mapModeSetting == SettingPackage.getInstance().getConfigKeyMapviewMode()) {
            callResponse = CallResponse.createSuccessResponse(
                    "当前已是" + ((TextUtils.equals(tts, "切换视图")) ? respTts : tts) + "模式"
            );
        } else {
            Log.d(IVrBridgeConstant.TAG, "Map view switch successfully !!! ");
            MapPackage.getInstance().switchMapMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, mapMode);
            SettingPackage.getInstance().setConfigKeyMapviewMode(mapModeSetting);
            callResponse = CallResponse.createSuccessResponse(
                    "已切换为" + ((TextUtils.equals(tts, "切换视图")) ? respTts : tts) + "模式"
            );
        }

        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 收藏位置
     *
     * @param poi          待收藏位置
     *                     LOCATION：当前定位；
     *                     DESTINATION：目的地/终点
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onFavoriteAdd(String poi, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onFavoriteAdd: poi = " + poi);
        if (TextUtils.isEmpty(poi)) {
            Log.e(IVrBridgeConstant.TAG, "addFavorite poi is empty");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("poi为空"));
            }
        }
        //todo Map未打开，打开地图并执行收藏

        if (IVrBridgeConstant.PoiType.DESTINATION.equals(poi)) {
            //引导目的地
            RouteParam routeParam = RoutePackage.getInstance().getEndPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP);
            if (null != routeParam) {
                PoiInfoEntity poiInfo = new PoiInfoEntity();
                poiInfo.setPid(routeParam.poiID);
                poiInfo.setName(routeParam.name);
                poiInfo.setAddress(routeParam.address);
                poiInfo.setAdCode(routeParam.adCode);
                if (null != routeParam.getRealPos()) {
                    poiInfo.setPoint(routeParam.getRealPos());
                }
                FavoriteInfo favoriteInfo = new FavoriteInfo();
                favoriteInfo.setCommonName(0);
                poiInfo.setFavoriteInfo(favoriteInfo);
                BehaviorPackage.getInstance().addFavorite(poiInfo);
                BehaviorPackage.getInstance().addFavoriteData(poiInfo, 0);
            }
        } else if (IVrBridgeConstant.PoiType.CURRENT_LOCATION.equals(poi)) {
            //当前定位
            LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null == locInfoBean || Double.compare(locInfoBean.getLongitude(), 0.0) <= 0
                    || Double.compare(locInfoBean.getLatitude(), 0.0) <= 0) {
                Log.e(IVrBridgeConstant.TAG, "curLocation is empty");
                if (null != respCallback) {
                    respCallback.onResponse(CallResponse.createFailResponse("不好意思，无法获取当前定位信息"));
                }
            } else {
                //搜索Poi详情
                GeoPoint geoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                VoiceSearchManager.getInstance().queryCurrentLocationDetail(IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE,
                        geoPoint, respCallback);
            }
        } else {
            //普通poi点
            VoiceSearchManager.getInstance().searchPoiInfo(IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE, poi, respCallback);
        }
        return CallResponse.createSuccessResponse();
    }

    /**
     * 打开地图收藏夹
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onFavoriteOpen(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onFavoriteOpen:");
        //todo 判断地图导航态与前台后台
        Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.FAVORITE_PAGE);
        MapPackage.getInstance().voiceOpenSearchPage(MapTypeId.MAIN_SCREEN_MAIN_MAP, bundle);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 打开导航搜索记录
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onSearchListOpen(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onSearchListOpen:");
        //todo 判断地图导航态与前台后台
        Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.SEARCH_HISTORY);
        MapPackage.getInstance().voiceOpenSearchPage(MapTypeId.MAIN_SCREEN_MAIN_MAP, bundle);
        return CallResponse.createSuccessResponse();
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
    public CallResponse onPoiSelect(String sessionId, String type, String typeValue, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onPoiSelect: sessionId = " + sessionId + ", type = " + type + ", typeValue = " + typeValue);
        boolean precess = true;
        switch (type) {
            case IVrBridgeConstant.PoiSelectType.POSITION:
                int index = Integer.parseInt(typeValue);
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

        if (!precess && null != respCallback) {
            respCallback.onResponse(CallResponse.createNotSupportResponse("不支持的筛选类型"));
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
    public CallResponse onPoiSort(String sessionId, String type, String rule, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onPoiSort: sessionId = " + sessionId + ", type = " + type + ", rule = " + rule);
        boolean isDescending = TextUtils.equals("DESCENDING", rule);
        VoiceSearchManager.getInstance().handlePoiSort(sessionId, type, isDescending, respCallback);
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
    public CallResponse onPoiPageChange(String sessionId, String type, String typeValue, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onPoiPageChange: sessionId = " + sessionId + ", type = " + type + ", typeValue = " + typeValue);
        return null;
    }

    /**
     * 导航多轮中断退出
     *
     * @param type 默认 DEFAULT。预留扩展，后续支持多种退出类型
     * @return CallResponse
     */
    @Override
    public CallResponse onMultiRoundExit(String type) {
        Log.d(IVrBridgeConstant.TAG, "onMultiRoundExit: type = " + type);
        return null;
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
    public CallResponse onMapToggle(String action, String target, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onMapToggle: action = " + action + ", target = " + target);
        return null;
    }

    /**
     * 修改日夜模式
     *
     * @param mode         1: 白天模式; 2: 黑夜模式;
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onDayNightModeChange(int mode, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onDayNightModeChange: mode = " + mode);
        CallResponse callResponse;

        //TODO 地图未打开，打开地图

        int dayNightMode = -1;
        switch (mode) {
            case 1:
                dayNightMode = MAP_DAY_MODE;
                break;
            case 2:
                dayNightMode = MAP_NIGHT_MODE;
                break;
            default:
                Log.d(IVrBridgeConstant.TAG, "Error mode changed !");
                callResponse = CallResponse.createFailResponse("切换失败，请重试");
                if (respCallback != null) {
                    respCallback.onResponse(callResponse);
                }
                return CallResponse.createSuccessResponse();
        }

        if (dayNightMode == SettingPackage.getInstance().getConfigKeyDayNightMode()) {
            //TODO 如果是后台切到前台
        } else {
            SettingPackage.getInstance().setConfigKeyDayNightMode(dayNightMode);
        }

        callResponse = CallResponse.createSuccessResponse("Switch "
                + (dayNightMode == MAP_DAY_MODE ? "day" : "night") + " mode successfully !");
        if (respCallback != null) {
            respCallback.onResponse(callResponse);
        }

        return CallResponse.createNotSupportResponse("Not support for now");
    }

    /**
     * 打开关闭登录页
     *
     * @param toggle       true: 打开; false: 关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onLoginPageToggle(boolean toggle, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onLoginPageToggle: toggle = " + toggle);
        return null;
    }

    /**
     * 创建队伍
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamCreate(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onNaviTeamCreate:");
        return CallResponse.createNotSupportResponse("not support team function");
    }

    /**
     * 退出队伍
     *
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamQuit(RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onNaviTeamQuit:");
        return CallResponse.createNotSupportResponse("not support team function");
    }

    /**
     * 打开关闭队伍全览
     *
     * @param toggle       true: 打开; false: 关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamPageToggle(boolean toggle, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onNaviTeamPageToggle: toggle = " + toggle);
        return CallResponse.createNotSupportResponse("not support team function");
    }

    /**
     * 打开关闭组队出行协议页面
     *
     * @param toggle       true: 打开; false: 关闭
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamAgreementPageToggle(boolean toggle, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onNaviTeamAgreementPageToggle: toggle = " + toggle);
        return CallResponse.createNotSupportResponse("not support team function");
    }

    /**
     * 加入队伍
     *
     * @param code         队伍口令
     * @param respCallback respCallback
     * @return CallResponse
     */
    @Override
    public CallResponse onNaviTeamJoin(String code, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "onNaviTeamJoin: code = " + code);
        return CallResponse.createNotSupportResponse("not support team function");
    }

    /**
     * 静音开关
     *
     * @param open     true：静音 false：关闭静音
     * @param callback 回调
     * @return CallResponse
     */
    @Override
    public CallResponse onMuteToggle(boolean open, RespCallback callback) {
        Log.d(IVrBridgeConstant.TAG, "onMuteToggle: open = " + open);
        CallResponse callResponse;

        //TODO 地图未打开，打开地图

        //映射静音状态
        int muteState = open ? 1 : 0;
        NaviPackage.getInstance().setMute(open);
        SettingPackage.getInstance().setConfigKeyMute(muteState);
        Log.d(IVrBridgeConstant.TAG, (open ? "Mute" : "Vocal") + " successfully");
        callResponse = CallResponse.createSuccessResponse((open ? "Mute" : "Vocal") + " successfully");

        if (callback != null) {
            callback.onResponse(callResponse);
        }
        return CallResponse.createSuccessResponse();
    }

    @Override
    public CallResponse onVolumeAdjust(NaviControlParam naviControlParam, RespCallback callback) {
        return CallResponse.createNotSupportResponse("地图不支持此功能");
    }
}
