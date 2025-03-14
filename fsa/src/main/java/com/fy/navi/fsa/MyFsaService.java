package com.fy.navi.fsa;

import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.content.res.Configuration;
import android.os.IBinder;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.WindowManager;

import com.android.utils.gson.GsonUtils;
import com.fy.navi.fsa.bean.DestInfo;
import com.fy.navi.fsa.bean.GeoPoint;
import com.fy.navi.fsa.bean.LaneLineInfo;
import com.fy.navi.fsa.bean.RemainInfo;
import com.fy.navi.fsa.bean.TurnInfo;
import com.fy.navi.fsa.scene.FsaNaviScene;
import com.fy.navi.hud.VTBinder;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.MapTypeId;
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
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.engine.IEngineObserver;
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

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class MyFsaService implements FsaServiceMethod.IRequestReceiveListener {

    private FSAService mService;
    private boolean mServiceInitSuccess;
    private int mTaskId;
    public final Map<String, String> FSA_EVENT_PAYLOAD_MAP = new HashMap<>();
    private boolean mIsHudInit = false;
    private boolean mIsHudServiceStart = false;
    private boolean mIsCruiseServiceStart = false;

    private VTBinder mBinder;
    private ServiceConnection mSrvConn = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName componentName, IBinder iBinder) {
            mBinder = (VTBinder) iBinder;
        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {
        }
    };

    public static MyFsaService getInstance() {
        return MyFsaServiceHolder.INSTANCE;
    }

    private static final class MyFsaServiceHolder {
        private static final MyFsaService INSTANCE = new MyFsaService();
    }

    private MyFsaService() {
        InetSocketAddress[] mAddress = new InetSocketAddress[1];
        mAddress[0] = new InetSocketAddress(FsaConstant.INET_CONFIG.SERVICE_IP, FsaConstant.INET_CONFIG.SERVICE_PORT);
        mService = new FSAService(mAddress, FsaConstant.INET_CONFIG.SERVICE_ID, 1, false);

        mService.addMethod(new FsaServiceMethod(MyFsaService.this));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_ROAD_NETWORK_MODE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_TBT_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_INTERVAL_SPEED_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_CRUISE_SPEED_LIMIT));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_LANE_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_REMAIN_TIME_DISTANCE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_DESTINATION_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_CURRENT_ROAD));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_SERVICE_AREA));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_TUNNEL_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_ENLARGE_ICON));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_RANGE_ON_ROUTE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_FORWARD_CAMERA));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_CONGESTION_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_TRAFFIC_MAP_MODE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_ROAD_CONDITION_INFO));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_PASSED_PERCENT));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_IN_NAVIGATION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_IN_LIGHT_NAVIGATION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_IN_CRUISE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_IN_NOP_NAVIGATION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_PANEL_STATUS));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_HUD_ENLARGE_MAP));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_FINGER_FLYING_HUD));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_HUD_SERVICE_INIT));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_THEME_CHANGED));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_NAVIGATION_STATUS));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_WHOLE_SPEED_LIMIT));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_NOP_NEXT_ROAD));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_CHANGE_DESTINATION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_SELF_DRIVING_POSITION));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_CHARGING_STATIONS_POI));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_PARKING_LOT_POI));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FSA_FUNCTION.ID_SERVICE_POI));

        mService.changeState(FSAService.ServiceState.INACTIVE);
    }

    /**
     * 根据service引擎初始化状态添加各个模块的数据监听.
     */
    public void init() {
        boolean engineInit = EnginePackage.getInstance().engineStatus();
        if (engineInit) {
            if (initFsaService()) {
                sendEvent(FsaConstant.FSA_FUNCTION.ID_ROAD_NETWORK_MODE, FsaConstant.FSA_VALUE.STRING_ONE);
            }
            addPackageListener();
        } else {
            EnginePackage.getInstance().addEngineObserver(FsaConstant.FSA_TAG, engineObserver);
        }

//        AppContext.mContext.registerReceiver(new BroadcastReceiver() {
//            @Override
//            public void onReceive(Context context, Intent intent) {
//                Log.d(FsaConstant.FSA_TAG, "onReceive: " + intent.getAction());
//                if (intent.getAction().equals("fsatest")) {
//                    onReceiveRequest(FsaConstant.FSA_METHOD.ID_REQUEST_MSG, intent.getStringExtra("payload"));
//                }
//            }
//        }, new IntentFilter("fsatest"), Context.RECEIVER_EXPORTED);
    }

    //Map引擎初始化监听
    private IEngineObserver engineObserver = new IEngineObserver() {
        @Override
        public void onInitEngineSuccess() {
            Log.d(FsaConstant.FSA_TAG, "serviceEngine init success");
            initFsaService();
            addPackageListener();
            sendEvent(FsaConstant.FSA_FUNCTION.ID_ROAD_NETWORK_MODE, FsaConstant.FSA_VALUE.STRING_ONE);
        }

        @Override
        public void onInitEngineFail(int code, String msg) {
            Log.e(FsaConstant.FSA_TAG, "engineInit error, code: " + code + ", msg: " + msg);
            sendEvent(FsaConstant.FSA_FUNCTION.ID_ROAD_NETWORK_MODE, FsaConstant.FSA_VALUE.STRING_ZERO);
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
    }

    public void initHudService(Context context, Intent intent) {
        try {
            WindowManager mWindowManager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
            DisplayMetrics metrics = new DisplayMetrics();
            mWindowManager.getDefaultDisplay().getMetrics(metrics);
        } catch (Exception e) {
            Log.e(FsaConstant.FSA_TAG, "MediaProjection error");
        }
        context.bindService(intent, mSrvConn, Context.BIND_AUTO_CREATE);
        context.startForegroundService(intent);
    }

    /**
     * 根据收到的payload不同，发送对应的Event给客户端.
     *
     * @param functionId 客户端主动请求信息的事件ID，当前唯一9201.
     * @param payload    客户端事件所带的参数，区分请求内容.
     */
    @Override
    public void onReceiveRequest(int functionId, String payload) {
        if (null == payload || payload.isEmpty()) {
            Log.e(FsaConstant.FSA_TAG, "onReceiveRequest: empty payload, can't send event");
            return;
        }
        Log.e(FsaConstant.FSA_TAG, "onReceiveRequest: " + payload + " - " + code2String(payload));
        if (FsaConstant.FSA_METHOD.ID_REQUEST_MSG == functionId) {
            switch (payload) {
                case FsaConstant.FSA_EVENT_PAYLOAD.ROAD_NETWORK_MODE:
                    // 获取地图当前是否存在有效的路网数据
                    sendEventToMap(FsaConstant.FSA_FUNCTION.ID_ROAD_NETWORK_MODE, FsaConstant.FSA_VALUE.STRING_ZERO);
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.TBT_INFO:
                    // 获取TBT信息
                    ArrayList<TurnInfo> laneInfoList = new ArrayList<>();
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_TBT_INFO, NaviStatus.NaviStatusType.NAVING, GsonUtils.toJson(laneInfoList));
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.INTERVAL_SPEED_INFO:
                    // 导航态区间限速信息
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_INTERVAL_SPEED_INFO, NaviStatus.NaviStatusType.NAVING, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.CRUISE_SPEED_LIMIT:
                    // 获取巡航态限速信息（非区间限速）
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_CRUISE_SPEED_LIMIT, NaviStatus.NaviStatusType.CRUISE, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.LANE_INFO:
                    // 获取车道线信息
                    LaneLineInfo laneLineInfo = new LaneLineInfo();
                    laneLineInfo.setShowType(-1);
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_LANE_INFO, NaviStatus.NaviStatusType.NAVING, GsonUtils.toJson(laneLineInfo));
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.REMAIN_TIME_DISTANCE:
                    // 获取导航态剩余距离、时间
                    RemainInfo remainInfo = new RemainInfo();
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_REMAIN_TIME_DISTANCE, NaviStatus.NaviStatusType.NAVING, GsonUtils.toJson(remainInfo));
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.DESTINATION_INFO:
                    // 获取导航目的地的名称和坐标
                    sendDestinationInfo();
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.CURRENT_ROAD:
                    // 获取当前路名
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_CURRENT_ROAD, NaviStatus.NaviStatusType.NAVING, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.SERVICE_AREA:
                    // 获取高速服务区信息
                    sendEventToMap(FsaConstant.FSA_FUNCTION.ID_SERVICE_AREA, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.TUNNEL_INFO:
                    // 获取隧道信息，高德没有接口单独透出隧道信息，只有在进出隧道前，TTS会播报
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.ENLARGE_ICON:
                    // 获取放大图信息
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_ENLARGE_ICON, NaviStatus.NaviStatusType.NAVING, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.RANGE_ON_ROUTE:
                    // 获取续航里程信息
                    ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos = RoutePackage.getInstance().getEvRangeOnRouteInfos();
                    FsaNaviScene.getInstance().updateEvRangeOnRouteInfo(MyFsaService.this, evRangeOnRouteInfos);
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.FORWARD_CAMERA:
                    // 获取导航态限速信息（非区间限速）
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_FORWARD_CAMERA, NaviStatus.NaviStatusType.NAVING, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.CONGESTION_INFO:
                    // 获取路况拥堵信息
                    sendEventToMap(FsaConstant.FSA_FUNCTION.ID_CONGESTION_INFO, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.TRAFFIC_MAP_MODE:
                    sendEvent(FsaConstant.FSA_FUNCTION.ID_TRAFFIC_MAP_MODE, FsaConstant.FSA_VALUE.STRING_ONE);
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.ROAD_CONDITION_INFO:
                    // 获取路况数据
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_ROAD_CONDITION_INFO, NaviStatus.NaviStatusType.NAVING, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.PASSED_PERCENT:
                    // 获取当前车标位置（用于绘制路况条）
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_PASSED_PERCENT, NaviStatus.NaviStatusType.NAVING, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.IN_NAVIGATION:
                    //获取地图是否处于专业导航状态
                    judgeMapCurStatus(FsaConstant.FSA_FUNCTION.ID_IN_NAVIGATION, NaviStatus.NaviStatusType.NAVING);
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.IN_LIGHT_NAVIGATION:
                    //获取地图是否处于轻导航状态
                    judgeMapCurStatus(FsaConstant.FSA_FUNCTION.ID_IN_LIGHT_NAVIGATION, NaviStatus.NaviStatusType.LIGHT_NAVING);
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.IN_CRUISE:
                    //获取地图是否处于巡航状态
                    judgeMapCurStatus(FsaConstant.FSA_FUNCTION.ID_IN_CRUISE, NaviStatus.NaviStatusType.CRUISE);
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.IN_NOP_NAVIGATION:
                    sendEvent(FsaConstant.FSA_FUNCTION.ID_IN_NOP_NAVIGATION, FsaConstant.FSA_VALUE.STRING_ZERO);
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.PANEL_STATUS:
                    sendEventToMap(FsaConstant.FSA_FUNCTION.ID_PANEL_STATUS, FsaConstant.FSA_VALUE.STRING_ZERO);
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.HUD_ENLARGE_MAP:
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_HUD_ENLARGE_MAP, NaviStatus.NaviStatusType.NAVING, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.OPEN_HUD_MAP:
                    if (mBinder == null) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": mBinder is null");
                        return;
                    }
                    if (mIsHudServiceStart || mIsCruiseServiceStart) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": vtserver has been started");
                        return;
                    }
                    mBinder.start();
                    mIsCruiseServiceStart = true;
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.CLOSE_HUD_MAP:
                    if (mBinder == null) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": mBinder is null");
                        return;
                    }
                    if (mIsHudServiceStart || !mIsCruiseServiceStart) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": vtserver has been stopped");
                        return;
                    }
                    mBinder.stop();
                    mIsCruiseServiceStart = false;
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.OPEN_HUD_VIDEO:
                    if (mBinder == null) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": mBinder is null");
                        return;
                    }
                    if (mIsHudServiceStart || mIsCruiseServiceStart) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": vtserver has been started");
                        return;
                    }
                    mBinder.start();
                    mIsHudServiceStart = true;
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.CLOSE_HUD_VIDEO:
                    if (mBinder == null) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": mBinder is null");
                        return;
                    }
                    if (!mIsHudServiceStart || mIsCruiseServiceStart) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": vtserver has been stopped");
                        return;
                    }
                    mBinder.stop();
                    mIsHudServiceStart = false;
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.OPEN_RSTP:
                    //todo 开启扶手屏
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.CLOSE_RSTP:
                    //todo 关闭扶手屏
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.HUD_SERVICE_INIT:
                    if (mBinder == null) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": mBinder is null");
                        return;
                    }
                    if (mIsHudInit) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": hud is initialized");
                        return;
                    }
                    mBinder.init();
                    FsaNaviScene.getInstance().updateHudVideInfo(MyFsaService.this);
                    mIsHudInit = true;
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.HUD_SERVICE_UNINIT:
                    if (mBinder == null) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": mBinder is null");
                        return;
                    }
                    if (!mIsHudInit) {
                        Log.e(FsaConstant.FSA_TAG, code2String(payload) + ": vtserver has been uninitialized");
                        return;
                    }
                    mBinder.uninit();
                    mIsHudInit = false;
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.THEME_CHANGED:
                    int configKeyDayNightMode = SettingPackage.getInstance().getConfigKeyDayNightMode();
                    if (configKeyDayNightMode == 17) { // 白天
                        sendEvent(FsaConstant.FSA_FUNCTION.ID_THEME_CHANGED, FsaConstant.FSA_VALUE.TRUE);
                    } else if (configKeyDayNightMode == 18) { // 黑夜
                        sendEvent(FsaConstant.FSA_FUNCTION.ID_THEME_CHANGED, FsaConstant.FSA_VALUE.FALSE);
                    } else {
                        sendEventToMap(FsaConstant.FSA_FUNCTION.ID_THEME_CHANGED, "");
                    }
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.NAVIGATION_STATUS:
                    //当前地图状态 1导航 2巡航 3模拟导航 4轻导航 0其他
                    NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
                    if (null != naviStatusPackage) {
                        String naviStatus = naviStatusPackage.getCurrentNaviStatus();
                        sendCurrentNaviStatus(naviStatus);
                    }
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.WHOLE_SPEED_LIMIT:
                    // 地图整体限速状态，不区分导航巡航. 区间非区间，变化和订阅时发送一次
                    sendEventToMap(FsaConstant.FSA_FUNCTION.ID_WHOLE_SPEED_LIMIT, "");
                    break;
                case FsaConstant.FSA_EVENT_PAYLOAD.NOP_NEXT_ROAD:
                    sendEventToMapCheckState(FsaConstant.FSA_FUNCTION.ID_NOP_NEXT_ROAD, NaviStatus.NaviStatusType.NAVING, "");
                    break;
                default:
                    Log.w(FsaConstant.FSA_TAG, "unHandle fsa payload: " + payload);
            }
        }
    }

    private void addPackageListener() {
        PositionPackage.getInstance().registerCallBack(mPositionCallback);
        NaviStatusPackage.getInstance().registerObserver(FsaConstant.FSA_TAG, mNaviStatusCallback);
        NaviPackage.getInstance().registerObserver(FsaConstant.FSA_TAG, mGuidanceObserver);
        CruisePackage.getInstance().registerObserver(FsaConstant.FSA_TAG, mCruiseObserver);
        MapPackage.getInstance().registerCallback(MapTypeId.MAIN_SCREEN_MAIN_MAP, mIMapPackageCallback);
        RoutePackage.getInstance().registerRouteObserver(FsaConstant.FSA_TAG, mRouteResultObserver);
        SearchPackage.getInstance().registerCallBack(FsaConstant.FSA_TAG, searchResultCallback);
    }

    /**
     * 发送数据到客户端.
     *
     * @param functionId 客户端订阅的事件ID.
     * @param info       使用payload装在的信息，json格式.
     */
    public void sendEvent(int functionId, String info) {
        sendEvent(functionId, info, true);
    }

    /**
     * 发送数据到客户端.
     *
     * @param functionId 客户端订阅的事件ID.
     * @param isSave     是否缓存.
     * @param info       使用payload装在的信息，json格式.
     */
    public void sendEvent(int functionId, String info, boolean isSave) {
        FsaServiceEvent event = (FsaServiceEvent) mService.eventHandler.getEventById(functionId);
        event.setOutputPayload(info.getBytes(StandardCharsets.UTF_8));
        Log.d(FsaConstant.FSA_TAG, "sendEvent: functionId = " + functionId + ", info = " + info);
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
    public void sendEventToMap(int functionId, String defaultValue) {
        sendEvent(functionId, FsaNaviScene.getInstance().getData(functionId, defaultValue), false);
    }

    /**
     * 发送缓存数据到客户端，判断导航是否处于某种状态.
     *
     * @param functionId   客户端订阅的事件ID.
     * @param naviState    导航状态.
     * @param defaultValue 默认数据.
     */
    public void sendEventToMapCheckState(int functionId, String naviState, String defaultValue) {
        if (naviState.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            sendEventToMap(functionId, defaultValue);
        } else {
            sendEvent(functionId, defaultValue, false);
        }
    }

    //定位信息
    private final IPositionPackageCallback mPositionCallback = new IPositionPackageCallback() {
        @Override
        public void onLocationInfo(LocInfoBean locationInfo) {
            FsaNaviScene.getInstance().updateCurrentCarLocation(MyFsaService.this, locationInfo);
        }
    };


    //状态回调
    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
            judgeMapCurStatus(FsaConstant.FSA_FUNCTION.ID_IN_NAVIGATION, NaviStatus.NaviStatusType.NAVING);
            judgeMapCurStatus(FsaConstant.FSA_FUNCTION.ID_IN_LIGHT_NAVIGATION, NaviStatus.NaviStatusType.LIGHT_NAVING);
            judgeMapCurStatus(FsaConstant.FSA_FUNCTION.ID_IN_CRUISE, NaviStatus.NaviStatusType.CRUISE);
            sendCurrentNaviStatus(naviStatus);
            if (NaviStatus.NaviStatusType.NAVING.equals(naviStatus)) {
                sendDestinationInfo();
                sendEvent(FsaConstant.FSA_FUNCTION.ID_PANEL_STATUS, FsaConstant.FSA_VALUE.STRING_ONE);
            } else {
                sendEvent(FsaConstant.FSA_FUNCTION.ID_PANEL_STATUS, FsaConstant.FSA_VALUE.STRING_ZERO);
            }
        }
    };

    private final SearchResultCallback searchResultCallback = new SearchResultCallback() {
        @Override
        public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
            if (mTaskId == taskId && searchResultEntity != null) {
                FsaNaviScene.getInstance().updateChargingStationInfo(MyFsaService.this, searchResultEntity);
            }
        }
    };

    //引导信息
    private final IGuidanceObserver mGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviInfo(NaviEtaInfo naviETAInfo) {
            FsaNaviScene.getInstance().updateTbtInfo(MyFsaService.this, naviETAInfo);
        }

        @Override
        public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
            FsaNaviScene.getInstance().updateLaneLineInfo(MyFsaService.this, isShowLane, laneInfoEntity);
        }

        @Override
        public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
            FsaNaviScene.getInstance().updateEnlargeMap(MyFsaService.this, isShowImage, naviImageInfo);
        }

        @Override
        public void onManeuverInfo(NaviManeuverInfo respData) {
            if (respData == null) {
                return;
            }
            FsaNaviScene.getInstance().updateNaviManeuverInfo(respData);
        }

        @Override
        public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
            FsaNaviScene.getInstance().updateTmcInfo(MyFsaService.this, naviTmcInfo);
        }

        @Override
        public void onNaviSpeedOverallInfo(SpeedOverallEntity speedCameraInfo) {
            if (null != speedCameraInfo && speedCameraInfo.getSpeedType() == NaviConstant.SpeedType.SPEED_OVERALL) {
                //区间测速动态信息更新
                FsaNaviScene.getInstance().updateIntervalCameraInfo(MyFsaService.this, speedCameraInfo);
            }
        }

        @Override
        public void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
            FsaNaviScene.getInstance().updateNavigationCameraInfo(MyFsaService.this, cameraInfo);
        }

        @Override
        public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
            FsaNaviScene.getInstance().updateHighwayService(MyFsaService.this, sapaInfoEntity);
        }

        @Override
        public void onCurrentRoadSpeed(int speed) {
            FsaNaviScene.getInstance().updateSpeedLimitSignData(MyFsaService.this, speed);
        }
    };


    //巡航信息
    private final ICruiseObserver mCruiseObserver = new ICruiseObserver() {
        public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
            FsaNaviScene.getInstance().updateCruiseInfoEntity(MyFsaService.this, cruiseInfoEntity);
        }
    };

    private final IMapPackageCallback mIMapPackageCallback = new IMapPackageCallback() {

        @Override
        public void onUiModeChanged(int uiMode) {
            boolean isNightMode = (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
            if (isNightMode) {
                sendEvent(FsaConstant.FSA_FUNCTION.ID_THEME_CHANGED, FsaConstant.FSA_VALUE.FALSE);
            } else {
                sendEvent(FsaConstant.FSA_FUNCTION.ID_THEME_CHANGED, FsaConstant.FSA_VALUE.TRUE);
            }
        }
    };

    private IRouteResultObserver mRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onRouteResult(RequestRouteResult requestRouteResult) {
            FsaNaviScene.getInstance().updateDestInfo(MyFsaService.this, requestRouteResult);

            if (null != requestRouteResult && null != requestRouteResult.getRouteLineInfos()
                    && !requestRouteResult.getRouteLineInfos().isEmpty()
                    && requestRouteResult.getRouteLineInfos().get(0) != null) {
                String elecRouteLabel = requestRouteResult.getRouteLineInfos().get(0).getElecRouteLabel();
                if ("不可达".equals(elecRouteLabel)) {
                    if (Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
                        mTaskId = SearchPackage.getInstance().aroundSearch(1, "充电站");
                    }
                }
            }
        }

        @Override
        public void onRouteRanges(ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {
            Log.d(FsaConstant.FSA_TAG, "onRouteRanges: ");
            FsaNaviScene.getInstance().updateEvRangeOnRouteInfo(MyFsaService.this, evRangeOnRouteInfos);
        }
    };


    //发送当前地图状态
    private void sendCurrentNaviStatus(String curStatus) {
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
        }

        sendEvent(FsaConstant.FSA_FUNCTION.ID_NAVIGATION_STATUS, String.valueOf(statusCode));
    }


    //发送导航目的地
    private void sendDestinationInfo() {
        RoutePackage routePackage = RoutePackage.getInstance();
        if (null != routePackage) {
            RouteParam routeParam = routePackage.getEndPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP);
            if (null != routeParam) {
                DestInfo destInfo = new DestInfo();
                destInfo.setName(routeParam.name);
                destInfo.setAddress(routeParam.address);
                if (null != routeParam.getRealPos()) {
                    GeoPoint location = new GeoPoint(routeParam.getRealPos().lat, routeParam.getRealPos().lon);
                    destInfo.setLocation(location);
                }
                sendEvent(FsaConstant.FSA_FUNCTION.ID_DESTINATION_INFO, GsonUtils.toJson(destInfo));
            }
        }
    }

    /**
     * 判断Map当前是否处于某种状态.
     *
     * @param functionId   发送的事件Id.
     * @param targetStatus 目标状态.
     */
    private void judgeMapCurStatus(int functionId, String targetStatus) {
        if (null == targetStatus || targetStatus.isEmpty()) {
            Log.e(FsaConstant.FSA_TAG, "targetStatus is empty");
            return;
        }
        NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
        if (null != naviStatusPackage) {
            String curStatus = naviStatusPackage.getCurrentNaviStatus();
            String value = FsaConstant.FSA_VALUE.STRING_ZERO;
            if (targetStatus.equals(curStatus)) {
                value = FsaConstant.FSA_VALUE.STRING_ONE;
            }
            sendEvent(functionId, value);
        }
    }

    private String code2String(String code) {
        if (FSA_EVENT_PAYLOAD_MAP.isEmpty()) {
            FSA_EVENT_PAYLOAD_MAP.put("0", "是否有有效路网数据"); // ROAD_NETWORK_MODE
            FSA_EVENT_PAYLOAD_MAP.put("1", "获取TBT信息"); // TBT_INFO
            FSA_EVENT_PAYLOAD_MAP.put("2", "导航态区间测速信息"); // INTERVAL_SPEED_INFO
            FSA_EVENT_PAYLOAD_MAP.put("3", "巡航态前方限速信息"); // CRUISE_SPEED_LIMIT
            FSA_EVENT_PAYLOAD_MAP.put("4", "车道线信息"); // LANE_INFO
            FSA_EVENT_PAYLOAD_MAP.put("5", "导航剩余时间和距离"); // REMAIN_TIME_DISTANCE
            FSA_EVENT_PAYLOAD_MAP.put("6", "导航目的地名称和坐标"); // DESTINATION_INFO
            FSA_EVENT_PAYLOAD_MAP.put("7", "当前道路名称"); // CURRENT_ROAD
            FSA_EVENT_PAYLOAD_MAP.put("8", "高速服务区信息"); // SERVICE_AREA
            FSA_EVENT_PAYLOAD_MAP.put("9", "获取隧道信息"); // TUNNEL_INFO
            FSA_EVENT_PAYLOAD_MAP.put("10", "获取放大图信息"); // ENLARGE_ICON
            FSA_EVENT_PAYLOAD_MAP.put("11", "获取续航里程信息"); // RANGE_ON_ROUTE
            FSA_EVENT_PAYLOAD_MAP.put("12", "导航态前方限速摄像头信息"); // FORWARD_CAMERA
            FSA_EVENT_PAYLOAD_MAP.put("13", "路况拥堵信息"); // CONGESTION_INFO
            FSA_EVENT_PAYLOAD_MAP.put("14", "当前路况显示模式"); // TRAFFIC_MAP_MODE
            FSA_EVENT_PAYLOAD_MAP.put("15", "当前路况数据"); // ROAD_CONDITION_INFO
            FSA_EVENT_PAYLOAD_MAP.put("16", "获取当前车标位置(已行驶里程占路线总长度百分比)"); // PASSED_PERCENT
            FSA_EVENT_PAYLOAD_MAP.put("17", "是否处于专业导航状态"); // IN_NAVIGATION
            FSA_EVENT_PAYLOAD_MAP.put("18", "是否处于轻导航状态"); // IN_LIGHT_NAVIGATION
            FSA_EVENT_PAYLOAD_MAP.put("19", "是否处于巡航状态"); // IN_CRUISE
            FSA_EVENT_PAYLOAD_MAP.put("20", "是否处于NOP导航"); // IN_NOP_NAVIGATION
            FSA_EVENT_PAYLOAD_MAP.put("21", "导航状态下诱导面板状态信息"); // PANEL_STATUS
            FSA_EVENT_PAYLOAD_MAP.put("22", "获取放大图信息(HUD专用)"); // HUD_ENLARGE_MAP
            FSA_EVENT_PAYLOAD_MAP.put("23", "开启仪表多屏地图渲染"); // OPEN_HUD_MAP
            FSA_EVENT_PAYLOAD_MAP.put("24", "关闭仪表多屏地图渲染"); // CLOSE_HUD_MAP
            FSA_EVENT_PAYLOAD_MAP.put("25", "开启HUD视频流服务"); // OPEN_HUD_VIDEO
            FSA_EVENT_PAYLOAD_MAP.put("26", "关闭HUD视频流服务"); // CLOSE_HUD_VIDEO
            FSA_EVENT_PAYLOAD_MAP.put("27", "开启扶手屏RSTP"); // OPEN_RSTP
            FSA_EVENT_PAYLOAD_MAP.put("28", "关闭扶手屏RSTP"); // CLOSE_RSTP
            FSA_EVENT_PAYLOAD_MAP.put("30", "HUD视频服务初始化"); // HUD_SERVICE_INIT
            FSA_EVENT_PAYLOAD_MAP.put("29", "HUD视频服务反初始化"); // HUD_SERVICE_UNINIT
            FSA_EVENT_PAYLOAD_MAP.put("31", "日夜模式切换"); // THEME_CHANGED
            FSA_EVENT_PAYLOAD_MAP.put("32", "当前地图状态"); // NAVIGATION_STATUS
            FSA_EVENT_PAYLOAD_MAP.put("33", "获取整体限速状态"); // WHOLE_SPEED_LIMIT
            FSA_EVENT_PAYLOAD_MAP.put("34", "获取NOP导航下个路段和下下个路段"); // NOP_NEXT_ROAD
        }
        return FSA_EVENT_PAYLOAD_MAP.get(code);
    }
}
