package com.fy.navi.fsa;

import android.app.ActivityOptions;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Matrix;
import android.hardware.display.DisplayManager;
import android.os.Build;
import android.view.Display;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.adas.JsonLog;
import com.fy.navi.fsa.bean.DestInfo;
import com.fy.navi.fsa.bean.GeoPoint;
import com.fy.navi.fsa.bean.LaneLineInfo;
import com.fy.navi.fsa.bean.RemainInfo;
import com.fy.navi.fsa.bean.TurnInfo;
import com.fy.navi.fsa.scene.FsaNaviScene;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.StartService;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.IPositionPackageCallback;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.gm.fsa.service.FSAService;
import com.gm.fsa.service.catalog.FSACatalog;
import com.iauto.vtserver.VTDescription;
import com.iauto.vtserver.VTServerBQJni;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public final class MyFsaService implements FsaServiceMethod.IRequestReceiveListener {
    private static final String TAG = "MyFsaService";

    private FSAService mService;
    private boolean mServiceInitSuccess;
    private int mTaskId;

    private boolean mIsHudInit = false;
    private boolean mIsHudServiceStart = false;
    private int mWidth = 328;
    private int mHeight = 172;

    private final Map<Integer, Set<String>> mSubscriberMap = new HashMap<>();

    public static MyFsaService getInstance() {
        return MyFsaServiceHolder.INSTANCE;
    }

    private static final class MyFsaServiceHolder {
        private static final MyFsaService INSTANCE = new MyFsaService();
    }

    private MyFsaService() {
        final InetSocketAddress[] address = new InetSocketAddress[1];
        address[0] = new InetSocketAddress(FsaConstant.InetConfig.SERVICE_IP, FsaConstant.InetConfig.SERVICE_PORT);
        mService = new FSAService(address, FsaConstant.InetConfig.SERVICE_ID, 1, false);

        mService.addMethod(new FsaServiceMethod(MyFsaService.this));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_ROAD_NETWORK_MODE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_TBT_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_INTERVAL_SPEED_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_CRUISE_SPEED_LIMIT));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_LANE_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_REMAIN_TIME_DISTANCE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_DESTINATION_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_CURRENT_ROAD));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_SERVICE_AREA));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_TUNNEL_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_ENLARGE_ICON));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_RANGE_ON_ROUTE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_FORWARD_CAMERA));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_CONGESTION_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_TRAFFIC_MAP_MODE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_ROAD_CONDITION_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_PASSED_PERCENT));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_IN_NAVIGATION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_IN_LIGHT_NAVIGATION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_IN_CRUISE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_IN_NOP_NAVIGATION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_PANEL_STATUS));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_HUD_ENLARGE_MAP));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_FINGER_FLYING_HUD));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_HUD_SERVICE_INIT));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_THEME_CHANGED));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_NAVIGATION_STATUS));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_WHOLE_SPEED_LIMIT));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_NOP_NEXT_ROAD));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_CHANGE_DESTINATION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_SELF_DRIVING_POSITION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_CHARGING_STATIONS_POI));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_PARKING_LOT_POI));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_SERVICE_POI));

        mService.changeState(FSAService.ServiceState.INACTIVE);
    }

    /**
     * 根据service引擎初始化状态添加各个模块的数据监听.
     */
    public void init() {
        final int engineInit = StartService.getInstance().getSdkActivation();
        if (-1 != engineInit) {
            if (initFsaService()) {
                sendEvent(FsaConstant.FsaFunction.ID_ROAD_NETWORK_MODE, FsaConstant.FsaValue.STRING_ONE);
            }
            addPackageListener();
        } else {
            StartService.getInstance().registerSdkCallback(mEngineObserver);
        }

//        AppContext.getInstance().getMContext().registerReceiver(new BroadcastReceiver() {
//            @Override
//            public void onReceive(Context context, Intent intent) {
//                Logger.d(FsaConstant.FSA_TAG, "onReceive: " + intent.getAction());
//                if (intent.getAction().equals("fsatest")) {
//                    onReceiveRequest(FsaConstant.FsaMethod.ID_REQUEST_MSG, intent.getStringExtra("payload"));
//                }
//            }
//        }, new IntentFilter("fsatest"), Context.RECEIVER_EXPORTED);
    }

    //Map引擎初始化监听
    private final StartService.ISdkInitCallback mEngineObserver = new StartService.ISdkInitCallback() {
        @Override
        public void onSdkInitSuccess() {
            Logger.d(FsaConstant.FSA_TAG, "serviceEngine init success");
            initFsaService();
            addPackageListener();
            sendEvent(FsaConstant.FsaFunction.ID_ROAD_NETWORK_MODE, FsaConstant.FsaValue.STRING_ONE);
        }

        @Override
        public void onSdkInitFail(final int code, final String msg) {
            Logger.e(FsaConstant.FSA_TAG, "engineInit error, code: " + code + ", msg: " + msg);
            sendEvent(FsaConstant.FsaFunction.ID_ROAD_NETWORK_MODE, FsaConstant.FsaValue.STRING_ZERO);
        }
    };


    /**
     * 初始化FSA服务端.
     *
     * @return initResult boolean.
     */
    private boolean initFsaService() {
        mServiceInitSuccess = mService.initialize();
        return mServiceInitSuccess;
    }


    /**
     * 销毁FSA服务.
     */
    public void destroyFsaService() {
        if (null != mService && mServiceInitSuccess) {
            mService.shutdown();
            mServiceInitSuccess = false;
        }
        StartService.getInstance().unregisterSdkCallback(mEngineObserver);
    }

    /**
     * 根据收到的payload不同，发送对应的Event给客户端.
     *
     * @param functionId 客户端主动请求信息的事件ID，当前唯一9201.
     * @param payload    客户端事件所带的参数，区分请求内容.
     */
    @Override
    public void onReceiveRequest(final int functionId, final String payload) {
        if (FsaConstant.FsaMethod.ID_REQUEST_MSG == functionId) {
            Logger.i(FsaConstant.FSA_TAG, "received method request: payload = " + payload + " - " + FsaIdString.event2String(payload));
            // checkStyle 方法太长故进行分割
            handlePayload1(payload);
            handlePayload2(payload);
        } else {
            Logger.v(FsaConstant.FSA_TAG, "received method request: other function = " + functionId);
        }
    }

    /**
     * 根据收到的payload不同，发送对应的Event给客户端.
     *
     * @param payload 客户端事件所带的参数，区分请求内容.
     */
    private void handlePayload1(final String payload) {
        switch (payload) {
            case FsaConstant.FsaEventPayload.OPEN_HUD_MAP:
                switchClusterActivity(true);
                break;
            case FsaConstant.FsaEventPayload.CLOSE_HUD_MAP:
                switchClusterActivity(false);
                break;
            case FsaConstant.FsaEventPayload.OPEN_HUD_VIDEO:
                if (mIsHudServiceStart) {
                    Logger.e(FsaConstant.FSA_TAG, FsaIdString.event2String(payload) + ": vtserver has been started");
                    return;
                }
                startHud();
                mIsHudServiceStart = true;
                break;
            case FsaConstant.FsaEventPayload.CLOSE_HUD_VIDEO:
                if (!mIsHudServiceStart) {
                    Logger.e(FsaConstant.FSA_TAG, FsaIdString.event2String(payload) + ": vtserver has been stopped");
                    return;
                }
                stopHud();
                mIsHudServiceStart = false;
                break;
            case FsaConstant.FsaEventPayload.OPEN_RSTP:
                //todo 开启扶手屏
                break;
            case FsaConstant.FsaEventPayload.CLOSE_RSTP:
                //todo 关闭扶手屏
                break;
            case FsaConstant.FsaEventPayload.HUD_SERVICE_INIT:
                if (mIsHudInit) {
                    Logger.e(FsaConstant.FSA_TAG, FsaIdString.event2String(payload) + ": hud is initialized");
                    return;
                }
                initHud();
                FsaNaviScene.getInstance().updateHudVideInfo(MyFsaService.this);
                mIsHudInit = true;
                break;
            case FsaConstant.FsaEventPayload.HUD_SERVICE_UNINIT:
                if (!mIsHudInit) {
                    Logger.e(FsaConstant.FSA_TAG, FsaIdString.event2String(payload) + ": vtserver has been uninitialized");
                    return;
                }
                uninitHud();
                mIsHudInit = false;
                break;
            case FsaConstant.FsaEventPayload.THEME_CHANGED:
                final int configKeyDayNightMode = SettingPackage.getInstance().getConfigKeyDayNightMode();
                if (configKeyDayNightMode == 17) { // 白天
                    sendEvent(FsaConstant.FsaFunction.ID_THEME_CHANGED, FsaConstant.FsaValue.TRUE);
                } else if (configKeyDayNightMode == 18) { // 黑夜
                    sendEvent(FsaConstant.FsaFunction.ID_THEME_CHANGED, FsaConstant.FsaValue.FALSE);
                } else {
                    sendEventToMap(FsaConstant.FsaFunction.ID_THEME_CHANGED, "");
                }
                break;
            case FsaConstant.FsaEventPayload.NAVIGATION_STATUS:
                //当前地图状态 1导航 2巡航 3模拟导航 4轻导航 0其他
                final NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
                if (null != naviStatusPackage) {
                    sendCurrentNaviStatus(naviStatusPackage.getCurrentNaviStatus());
                }
                break;
            case FsaConstant.FsaEventPayload.WHOLE_SPEED_LIMIT:
                // 地图整体限速状态，不区分导航巡航. 区间非区间，变化和订阅时发送一次
                sendEventToMap(FsaConstant.FsaFunction.ID_WHOLE_SPEED_LIMIT, "");
                break;
            case FsaConstant.FsaEventPayload.NOP_NEXT_ROAD:
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_NOP_NEXT_ROAD, NaviStatus.NaviStatusType.NAVING, "");
                break;
            default:
        }
    }

    /**
     * 处理客户端发送过来的事件.
     *
     * @param payload 客户端事件所带的参数，区分请求内容.
     */
    private void handlePayload2(final String payload) {
        switch (payload) {
            case FsaConstant.FsaEventPayload.ROAD_NETWORK_MODE:
                // 获取地图当前是否存在有效的路网数据
                sendEventToMap(FsaConstant.FsaFunction.ID_ROAD_NETWORK_MODE, FsaConstant.FsaValue.STRING_ZERO);
                break;
            case FsaConstant.FsaEventPayload.TBT_INFO:
                // 获取TBT信息
                final ArrayList<TurnInfo> laneInfoList = new ArrayList<>();
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_TBT_INFO, NaviStatus.NaviStatusType.NAVING, GsonUtils.toJson(laneInfoList));
                break;
            case FsaConstant.FsaEventPayload.INTERVAL_SPEED_INFO:
                // 导航态区间限速信息
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_INTERVAL_SPEED_INFO, NaviStatus.NaviStatusType.NAVING, "");
                break;
            case FsaConstant.FsaEventPayload.CRUISE_SPEED_LIMIT:
                // 获取巡航态限速信息（非区间限速）
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_CRUISE_SPEED_LIMIT, NaviStatus.NaviStatusType.CRUISE, "");
                break;
            case FsaConstant.FsaEventPayload.LANE_INFO:
                // 获取车道线信息
                final LaneLineInfo laneLineInfo = new LaneLineInfo();
                laneLineInfo.setShowType(-1);
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_LANE_INFO, NaviStatus.NaviStatusType.NAVING, GsonUtils.toJson(laneLineInfo));
                break;
            case FsaConstant.FsaEventPayload.REMAIN_TIME_DISTANCE:
                // 获取导航态剩余距离、时间
                final RemainInfo remainInfo = new RemainInfo();
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_REMAIN_TIME_DISTANCE,
                        NaviStatus.NaviStatusType.NAVING, GsonUtils.toJson(remainInfo));
                break;
            case FsaConstant.FsaEventPayload.DESTINATION_INFO:
                // 获取导航目的地的名称和坐标
                sendDestinationInfo();
                break;
            case FsaConstant.FsaEventPayload.CURRENT_ROAD:
                //1.1.8 获取当前路名
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_CURRENT_ROAD, NaviStatus.NaviStatusType.NAVING, "");
                break;
            case FsaConstant.FsaEventPayload.SERVICE_AREA:
                // 获取高速服务区信息
                sendEventToMap(FsaConstant.FsaFunction.ID_SERVICE_AREA, "");
                break;
            case FsaConstant.FsaEventPayload.TUNNEL_INFO:
                // 获取隧道信息，高德没有接口单独透出隧道信息，只有在进出隧道前，TTS会播报
                break;
            case FsaConstant.FsaEventPayload.ENLARGE_ICON:
                // 获取放大图信息
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_ENLARGE_ICON, NaviStatus.NaviStatusType.NAVING, "");
                break;
            case FsaConstant.FsaEventPayload.RANGE_ON_ROUTE:
                // 获取续航里程信息
                final ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos = RoutePackage.getInstance().getEvRangeOnRouteInfos();
                FsaNaviScene.getInstance().updateEvRangeOnRouteInfo(MyFsaService.this, evRangeOnRouteInfos);
                break;
            case FsaConstant.FsaEventPayload.FORWARD_CAMERA:
                // 获取导航态限速信息（非区间限速）
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_FORWARD_CAMERA, NaviStatus.NaviStatusType.NAVING, "");
                break;
            case FsaConstant.FsaEventPayload.CONGESTION_INFO:
                // 获取路况拥堵信息
                sendEventToMap(FsaConstant.FsaFunction.ID_CONGESTION_INFO, "");
                break;
            case FsaConstant.FsaEventPayload.TRAFFIC_MAP_MODE:
                sendEvent(FsaConstant.FsaFunction.ID_TRAFFIC_MAP_MODE, FsaConstant.FsaValue.STRING_ONE);
                break;
            case FsaConstant.FsaEventPayload.ROAD_CONDITION_INFO:
                // 获取路况数据
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_ROAD_CONDITION_INFO, NaviStatus.NaviStatusType.NAVING, "");
                break;
            case FsaConstant.FsaEventPayload.PASSED_PERCENT:
                // 获取当前车标位置（用于绘制路况条）
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_PASSED_PERCENT, NaviStatus.NaviStatusType.NAVING, "");
                break;
            case FsaConstant.FsaEventPayload.IN_NAVIGATION:
                //获取地图是否处于专业导航状态
                judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_NAVIGATION, NaviStatus.NaviStatusType.NAVING);
                break;
            case FsaConstant.FsaEventPayload.IN_LIGHT_NAVIGATION:
                //获取地图是否处于轻导航状态
                judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_LIGHT_NAVIGATION, NaviStatus.NaviStatusType.LIGHT_NAVING);
                break;
            case FsaConstant.FsaEventPayload.IN_CRUISE:
                //获取地图是否处于巡航状态
                judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_CRUISE, NaviStatus.NaviStatusType.CRUISE);
                break;
            case FsaConstant.FsaEventPayload.IN_NOP_NAVIGATION:
                sendEvent(FsaConstant.FsaFunction.ID_IN_NOP_NAVIGATION, FsaConstant.FsaValue.STRING_ZERO);
                break;
            case FsaConstant.FsaEventPayload.PANEL_STATUS:
                sendEventToMap(FsaConstant.FsaFunction.ID_PANEL_STATUS, FsaConstant.FsaValue.STRING_ZERO);
                break;
            case FsaConstant.FsaEventPayload.HUD_ENLARGE_MAP:
                sendEventToMapCheckState(FsaConstant.FsaFunction.ID_HUD_ENLARGE_MAP, NaviStatus.NaviStatusType.NAVING, "");
                break;
            default:
        }
    }

    /**
     * 添加监听器.
     */
    private void addPackageListener() {
        PositionPackage.getInstance().registerCallBack(mPositionCallback);
        NaviStatusPackage.getInstance().registerObserver(FsaConstant.FSA_TAG, mNaviStatusCallback);
        NaviPackage.getInstance().registerObserver(FsaConstant.FSA_TAG, mGuidanceObserver);
        CruisePackage.getInstance().registerObserver(FsaConstant.FSA_TAG, mCruiseObserver);
        MapPackage.getInstance().registerCallback(MapType.MAIN_SCREEN_MAIN_MAP, mIMapPackageCallback);
        RoutePackage.getInstance().registerRouteObserver(FsaConstant.FSA_TAG, mRouteResultObserver);
        SearchPackage.getInstance().registerCallBack(FsaConstant.FSA_TAG, mSearchResultCallback);
    }

    /**
     * 发送数据到客户端.
     *
     * @param functionId 客户端订阅的事件ID.
     * @param info       使用payload装在的信息，json格式.
     */
    public void sendEvent(final int functionId, final String info) {
        sendEvent(functionId, info, true);
    }

    /**
     * 发送数据到客户端.
     *
     * @param functionId 客户端订阅的事件ID.
     * @param isSave     是否缓存.
     * @param info       使用payload装在的信息，json格式.
     */
    public void sendEvent(final int functionId, final String info, final boolean isSave) {
        if (!checkSubscribe(functionId)) {
            Logger.v(FsaConstant.FSA_TAG, "sendEvent: not subscribe functionId = " + functionId);
            return;
        }
        final FsaServiceEvent event = (FsaServiceEvent) mService.eventHandler.getEventById(functionId);
        event.setOutputPayload(info.getBytes(StandardCharsets.UTF_8));
        JsonLog.saveJsonToCache(info, "fsa.json", functionId + "-" + FsaIdString.function2String(functionId));
        if (functionId == FsaConstant.FsaFunction.ID_ENLARGE_ICON || functionId == FsaConstant.FsaFunction.ID_HUD_ENLARGE_MAP) {
            Logger.d(FsaConstant.FSA_TAG, "sendEvent: " + functionId + "-" + FsaIdString.function2String(functionId));
        } else if (functionId == FsaConstant.FsaFunction.ID_FINGER_FLYING_HUD) {//三指飞屏
            Logger.d(FsaConstant.FSA_TAG, "sendEvent: " + functionId + "-" + FsaIdString.function2String(functionId));
        }else {
            Logger.d(FsaConstant.FSA_TAG, "sendEvent: " + functionId + "-" + FsaIdString.function2String(functionId) + ", info = " + info);
        }
        mService.eventHandler.sendEvent(event, FSACatalog.DeviceName.UNKNOWN);
        if (isSave) {
            FsaNaviScene.getInstance().saveData(functionId, info);
        }
    }

    /**
     * 发送缓存数据到客户端.
     *
     * @param functionId   客户端订阅的事件ID.
     * @param defaultValue 默认数据.
     */
    public void sendEventToMap(final int functionId, final String defaultValue) {
        sendEvent(functionId, FsaNaviScene.getInstance().getData(functionId, defaultValue), false);
    }

    /**
     * 发送缓存数据到客户端，判断导航是否处于某种状态.
     *
     * @param functionId   客户端订阅的事件ID.
     * @param naviState    导航状态.
     * @param defaultValue 默认数据.
     */
    public void sendEventToMapCheckState(final int functionId, final String naviState, final String defaultValue) {
        if (naviState.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            sendEventToMap(functionId, defaultValue);
        } else {
            sendEvent(functionId, defaultValue, false);
        }
    }

    //定位信息
    private final IPositionPackageCallback mPositionCallback = new IPositionPackageCallback() {
        @Override
        public void onLocationInfo(final LocInfoBean locationInfo) {
            FsaNaviScene.getInstance().updateCurrentCarLocation(MyFsaService.this, locationInfo);
        }
    };


    //状态回调
    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(final String naviStatus) {
            judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_NAVIGATION, NaviStatus.NaviStatusType.NAVING);
            judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_LIGHT_NAVIGATION, NaviStatus.NaviStatusType.LIGHT_NAVING);
            judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_CRUISE, NaviStatus.NaviStatusType.CRUISE);
            sendCurrentNaviStatus(naviStatus);
            if (NaviStatus.NaviStatusType.NAVING.equals(naviStatus)) {
                sendDestinationInfo();
                sendEvent(FsaConstant.FsaFunction.ID_PANEL_STATUS, FsaConstant.FsaValue.STRING_ONE);
            } else {
                sendEvent(FsaConstant.FsaFunction.ID_PANEL_STATUS, FsaConstant.FsaValue.STRING_ZERO);
            }
        }
    };

    /**
     * 搜索结果回调.
     */
    private final SearchResultCallback mSearchResultCallback = new SearchResultCallback() {
        @Override
        public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
            if (mTaskId == taskId && searchResultEntity != null) {
                FsaNaviScene.getInstance().updateChargingStationInfo(MyFsaService.this, searchResultEntity);
            }
        }
    };

    //引导信息
    private final IGuidanceObserver mGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
            FsaNaviScene.getInstance().updateTbtInfo(MyFsaService.this, naviETAInfo);
        }

        @Override
        public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
            FsaNaviScene.getInstance().updateLaneLineInfo(MyFsaService.this, isShowLane, laneInfoEntity);
        }

        @Override
        public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
            FsaNaviScene.getInstance().updateEnlargeMap(MyFsaService.this, isShowImage, naviImageInfo);
        }

        @Override
        public void onManeuverInfo(final NaviManeuverInfo respData) {
            if (respData == null) {
                return;
            }
            FsaNaviScene.getInstance().updateNaviManeuverInfo(respData);
        }

        @Override
        public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
            FsaNaviScene.getInstance().updateTmcInfo(MyFsaService.this, naviTmcInfo);
        }

        @Override
        public void onNaviSpeedOverallInfo(final SpeedOverallEntity speedCameraInfo) {
            if (null != speedCameraInfo && speedCameraInfo.getSpeedType() == NaviConstant.SpeedType.SPEED_OVERALL) {
                //区间测速动态信息更新
                FsaNaviScene.getInstance().updateIntervalCameraInfo(MyFsaService.this, speedCameraInfo);
            }
        }

        @Override
        public void onNaviCameraInfo(final CameraInfoEntity cameraInfo) {
            FsaNaviScene.getInstance().updateNavigationCameraInfo(MyFsaService.this, cameraInfo);
        }

        @Override
        public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
            FsaNaviScene.getInstance().updateHighwayService(MyFsaService.this, sapaInfoEntity);
        }

        @Override
        public void onCurrentRoadSpeed(final int speed) {
            FsaNaviScene.getInstance().updateSpeedLimitSignData(MyFsaService.this, speed);
        }
    };


    //巡航信息
    private final ICruiseObserver mCruiseObserver = new ICruiseObserver() {
        public void onShowCruiseCameraExt(final CruiseInfoEntity cruiseInfoEntity) {
            FsaNaviScene.getInstance().updateCruiseInfoEntity(MyFsaService.this, cruiseInfoEntity);
        }
    };

    private final IMapPackageCallback mIMapPackageCallback = new IMapPackageCallback() {

        @Override
        public void onUiModeChanged(final ThemeType uiMode) {
            if (uiMode == ThemeType.NIGHT) {
                sendEvent(FsaConstant.FsaFunction.ID_THEME_CHANGED, FsaConstant.FsaValue.FALSE);
            } else {
                sendEvent(FsaConstant.FsaFunction.ID_THEME_CHANGED, FsaConstant.FsaValue.TRUE);
            }
        }

        @Override
        public void onEGLScreenshot(MapType mapTypeId, byte[] bytes) {
            if (!mIsHudServiceStart) {
                return;
            }
            byte[] bytes1 = processPicture(bytes);
            VTServerBQJni.getInstance().nativeNotifyVideoData(bytes1);
            // 以下是调试用
//            Bitmap bitmap = Bitmap.createBitmap(328, 172, Bitmap.Config.ARGB_8888);
//            bitmap.copyPixelsFromBuffer(ByteBuffer.wrap(bytes1));

//            // 创建文件保存图片
//            File file = new File(AppContext.getInstance().getMContext().getCacheDir(), "image.png");
//            try (FileOutputStream fos = new FileOutputStream(file)) {
//                bitmap.compress(Bitmap.CompressFormat.PNG, 100, fos);
//                Log.d(TAG, "Image saved successfully: " + file.getAbsolutePath());
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
        }
    };

    private IRouteResultObserver mRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onRouteResult(final RequestRouteResult requestRouteResult) {
            FsaNaviScene.getInstance().updateDestInfo(MyFsaService.this, requestRouteResult);

            if (null != requestRouteResult && null != requestRouteResult.getMRouteLineInfos()
                    && !requestRouteResult.getMRouteLineInfos().isEmpty()
                    && requestRouteResult.getMRouteLineInfos().get(0) != null) {
                final String elecRouteLabel = requestRouteResult.getMRouteLineInfos().get(0).getMElecRouteLabel();
                if ("不可达".equals(elecRouteLabel)) {
                    if (Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
                        mTaskId = SearchPackage.getInstance().aroundSearch(1, "充电站");
                    }
                }
            }
        }

        @Override
        public void onRouteRanges(final ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {
            Logger.d(FsaConstant.FSA_TAG, "onRouteRanges: ");
            FsaNaviScene.getInstance().updateEvRangeOnRouteInfo(MyFsaService.this, evRangeOnRouteInfos);
        }
    };


    /**
     * 发送当前导航状态.
     *
     * @param curStatus 当前导航状态
     */
    private void sendCurrentNaviStatus(final String curStatus) {
        int statusCode = 0;
        switch (curStatus) {
            case NaviStatus.NaviStatusType.NAVING:
                statusCode = 1;
                break;
            case NaviStatus.NaviStatusType.CRUISE:
                statusCode = 2;
                break;
            case NaviStatus.NaviStatusType.LIGHT_NAVING:
                statusCode = 4;
                break;
            default:
        }

        sendEvent(FsaConstant.FsaFunction.ID_NAVIGATION_STATUS, String.valueOf(statusCode));
    }


    /**
     * 发送目的地信息.
     */
    private void sendDestinationInfo() {
        final RoutePackage routePackage = RoutePackage.getInstance();
        if (null != routePackage) {
            final RouteParam routeParam = routePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
            if (null != routeParam) {
                final DestInfo destInfo = new DestInfo();
                destInfo.setName(routeParam.getName());
                destInfo.setAddress(routeParam.getAddress());
                if (null != routeParam.getRealPos()) {
                    final GeoPoint location = new GeoPoint(routeParam.getRealPos().getLon(), routeParam.getRealPos().getLat());
                    destInfo.setLocation(location);
                }
                sendEvent(FsaConstant.FsaFunction.ID_DESTINATION_INFO, GsonUtils.toJson(destInfo));
            }
        }
    }

    /**
     * 判断Map当前是否处于某种状态.
     *
     * @param functionId   发送的事件Id.
     * @param targetStatus 目标状态.
     */
    private void judgeMapCurStatus(final int functionId, final String targetStatus) {
        if (null == targetStatus || targetStatus.isEmpty()) {
            Logger.e(FsaConstant.FSA_TAG, "targetStatus is empty");
            return;
        }
        final NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
        if (null != naviStatusPackage) {
            final String curStatus = naviStatusPackage.getCurrentNaviStatus();
            String value = FsaConstant.FsaValue.STRING_ZERO;
            if (targetStatus.equals(curStatus)) {
                value = FsaConstant.FsaValue.STRING_ONE;
            }
            sendEvent(functionId, value);
        }
    }

    /**
     * 检查是否订阅了某个事件.
     *
     * @param functionId 事件Id.
     * @return true: 订阅了. false: 没有订阅.
     */
    private boolean checkSubscribe(final int functionId) {
        if (mSubscriberMap.containsKey(functionId)) {
            final Set<String> ipSet = mSubscriberMap.get(functionId);
            return null != ipSet;
        } else {
            return false;
        }
    }

    /**
     * 订阅事件.
     *
     * @param functionId 事件Id.
     * @param ip         ip地址.
     */
    public void subscribeEvent(final int functionId, final String ip) {
        if (mSubscriberMap.containsKey(functionId)) {
            final Set<String> ipSet = mSubscriberMap.get(functionId);
            if (null != ipSet) {
                ipSet.add(ip);
            }
        } else {
            final Set<String> ipSet = new HashSet<>();
            ipSet.add(ip);
            mSubscriberMap.put(functionId, ipSet);
        }
    }

    /**
     * 取消订阅事件.
     *
     * @param functionId 事件Id.
     * @param ip         ip地址.
     */
    public void unsubscribeEvent(final int functionId, final String ip) {
        if (mSubscriberMap.containsKey(functionId)) {
            final Set<String> ipSet = mSubscriberMap.get(functionId);
            if (null != ipSet) {
                ipSet.remove(ip);
                if (ipSet.isEmpty()) {
                    mSubscriberMap.remove(functionId);
                }
            }
        }
    }

    /**
     * 切换cluster activity.
     *
     * @param isOpen true: 打开. false: 关闭.
     */
    private void switchClusterActivity(final boolean isOpen) {
        int secondeDid = 2; // 仪表的DisplayId
        if (!"gm".equals(Build.MANUFACTURER)) { // 如果是非车机环境
            final DisplayManager displayManager = AppContext.getInstance().getMContext().getSystemService(DisplayManager.class);
            for (Display display : displayManager.getDisplays()) {
                Logger.d(FsaConstant.FSA_TAG, "dispaly: " + display.getName() + ", id " + display.getDisplayId() + " :" + display);
                if (display.getDisplayId() != 0) {
                    secondeDid = display.getDisplayId();
                    break;
                }
            }
        }
        if (isOpen) {
            Logger.d(FsaConstant.FSA_TAG, "open ClusterActivity");
            final ActivityOptions options = ActivityOptions.makeBasic();
            options.setLaunchDisplayId(secondeDid);
            final Intent intent = new Intent();
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setAction("com.fy.navi.hmi.cluster_map.ClusterActivity");
            intent.putExtra("isOpen", isOpen);
            AppContext.getInstance().getMContext().startActivity(intent, options.toBundle());
        } else {
            Logger.d(FsaConstant.FSA_TAG, "close ClusterActivity");
            Intent closeIntent = new Intent("com.fy.navi.hmi.cluster_map.ClusterActivity");
            AppContext.getInstance().getMContext().sendBroadcast(closeIntent);
        }
    }

    private byte[] processPicture(byte[] bytes) {
        if (!NaviStatus.NaviStatusType.NAVING.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            return new byte[225664];
        }
        int width = 328;
        int height = 172;
        int pixelSize = 4;

        Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
        bitmap.copyPixelsFromBuffer(ByteBuffer.wrap(bytes));
        // 翻转图像
        Matrix matrix = new Matrix();
        matrix.postScale(1, -1);
        matrix.postTranslate(bitmap.getWidth(), bitmap.getHeight());
        Bitmap flippedBitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix, true);

        // 将翻转后的 Bitmap 转换为 byte[]
        ByteBuffer flippedBuffer = ByteBuffer.allocate(flippedBitmap.getByteCount());
        flippedBitmap.copyPixelsToBuffer(flippedBuffer);
        byte[] flippedBytes = flippedBuffer.array();

        // 提取有效数据
        int rowStride = ((width * pixelSize + 3) / 4) * 4;
        int validRowBytes = width * pixelSize;
        byte[] croppedData = new byte[width * height * pixelSize];
        ByteBuffer buffer = ByteBuffer.wrap(flippedBytes);
        for (int i = 0; i < height; i++) {
            int bufferPosition = i * rowStride;
            int arrayPosition = i * validRowBytes;
            buffer.position(bufferPosition);
            buffer.get(croppedData, arrayPosition, validRowBytes);
        }
        return croppedData;
    }

    /**
     * 初始化
     */
    public void initHud() {
        Logger.d(TAG, "service init");
        if (!VTServerBQJni.getInstance().isIsSuccessLoadLibrary()) {
            Logger.d(TAG, "the so library failed to load");
            return;
        }
        final int ret = VTServerBQJni.getInstance().nativeInitialize();
        Logger.d(TAG, "NativeInitialize ret is " + ret);
        final VTDescription description = new VTDescription();
        description.width = mWidth;
        description.height = mHeight;
        description.videoFormat = 0x901001;  // PixelFormat.RGBA_8888;
        VTServerBQJni.getInstance().nativeSetVideoDescription(description);
        Logger.d(TAG, "NativeSetVideoDescription");
    }

    /**
     * 开始
     */
    public void startHud() {
        Logger.d(TAG, "service start");
        final int ret = VTServerBQJni.getInstance().nativeStart();
        Logger.d(TAG, "service start NativeStart ret is " + ret);
    }

    /**
     * 停止
     */
    public void stopHud() {
        Logger.d(TAG, "service stop");
        final int ret = VTServerBQJni.getInstance().nativeStop();
        Logger.d(TAG, "service stop NativeStop ret is " + ret);
    }

    /**
     * 销毁
     */
    public void uninitHud() {
        Logger.d(TAG, "service uninit");
        if (!VTServerBQJni.getInstance().isIsSuccessLoadLibrary()) {
            Logger.d(TAG, "the so library failed to load");
            return;
        }
        VTServerBQJni.getInstance().nativeUninitialize();
    }
}
