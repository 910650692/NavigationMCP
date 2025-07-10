package com.sgm.navi.fsa;

import android.app.ActivityOptions;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Matrix;
import android.graphics.Rect;
import android.os.Handler;
import android.os.Looper;

import androidx.annotation.WorkerThread;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.adas.JsonLog;
import com.sgm.navi.fsa.bean.DestInfo;
import com.sgm.navi.fsa.bean.GeoPoint;
import com.sgm.navi.fsa.bean.LaneLineInfo;
import com.sgm.navi.fsa.bean.RemainInfo;
import com.sgm.navi.fsa.bean.TrafficLightInfo;
import com.sgm.navi.fsa.bean.TurnInfo;
import com.sgm.navi.fsa.scene.FsaNaviScene;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.CameraInfoEntity;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.LightInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navi.TrafficLightCountdownEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.route.EvRangeOnRouteInfo;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.utils.BevPowerCarUtils;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.hud.HudPackage;
import com.sgm.navi.service.logicpaket.hud.IHudCallback;
import com.sgm.navi.service.logicpaket.l2.L2Package;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.signal.SignalCallback;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;
import com.sgm.navi.utils.ActivityCloseManager;
import com.sgm.navi.utils.ClusterMapOpenCloseManager;
import com.sgm.navi.utils.ThreeFingerFlyingScreenListener;
import com.sgm.navi.utils.ThreeFingerFlyingScreenManager;
import com.gm.fsa.service.FSAService;
import com.gm.fsa.service.catalog.FSACatalog;
import com.iauto.vtserver.VTDescription;
import com.iauto.vtserver.VTServerBQJni;

import java.io.ByteArrayOutputStream;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public final class MyFsaService implements FsaServiceMethod.IRequestReceiveListener , ThreeFingerFlyingScreenListener {
    private static final String TAG = "MyFsaService";

    private FSAService mService;
    private boolean mServiceInitSuccess;
    private int mChargeStationTaskId;
    private int mParkingLotTaskId;
    private int mGasStationTaskId;
    private ScheduledFuture mChargeStationSF;
    private ScheduledFuture mParkingLotSF;
    private ScheduledFuture mServiceSF;
    private ScheduledFuture mGasStationSF;
    private SapaInfoEntity mSapaInfoEntity;

    private boolean mIsHudInit = false;
    private boolean mIsHudServiceStart = false;
    private int mWidth = 328;
    private int mHeight = 172;

    private long lastProcessTime = 0;
    private static final long MIN_INTERVAL = 600;
    private final Queue<Bitmap> mBitmapPool = new LinkedList<>();
    private final Object mLock = new Object();
    private static final int IS_GB = 1;
    private static final int IS_CLEA = 0;
    private static final int IS_BUICK = 1;
    private static final int IS_CADILLAC = 2;

    private ExportEventCallBack mEventCallBack;

    private final Map<Integer, Set<String>> mSubscriberMap = new HashMap<>();

    public static MyFsaService getInstance() {
        return MyFsaServiceHolder.INSTANCE;
    }

    private final Handler handler = new Handler(Looper.getMainLooper(), msg -> {
        // 根据消息类型处理不同的事件
        switch (msg.what) {
            case 0:
                switchClusterActivity(true);
                break;
            case 1:
                switchClusterActivity(false);
                break;
        }
        return true;
    });

    @Override
    public void onThreeFingerFlyingScreenCall(boolean isLeft) {
        if (isLeft){
            Logger.d(TAG, "triggerClose: 触发左飞屏");
            MyFsaService.getInstance().sendEvent(FsaConstant.FsaFunction.ID_FINGER_FLYING_HUD, "2");
        }else {
            Logger.d(TAG, "triggerClose: 触发右关屏");
            MyFsaService.getInstance().sendEvent(FsaConstant.FsaFunction.ID_FINGER_FLYING_HUD, "3");
        }
    }

    private static final class MyFsaServiceHolder {
        private static final MyFsaService INSTANCE = new MyFsaService();
    }

    private MyFsaService() {
        final InetSocketAddress[] address = new InetSocketAddress[1];
        String serviceIp = CalibrationPackage.getInstance().architecture() == IS_CLEA ? FsaConstant.InetConfig.CLEA_SERVICE_IP :FsaConstant.InetConfig.GB_SERVICE_IP;
        address[0] = new InetSocketAddress(serviceIp, FsaConstant.InetConfig.SERVICE_PORT);
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
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_VISIBLE_ENLARGE));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_GAS_STATION_POI));
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_SERVICE_HOLE));//挖洞
        mService.addEvent(new FsaServiceEvent(FsaConstant.FsaFunction.ID_TRAFFIC_LIGHT_INFO));

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
                activelySendNavigationStatus();
            }
            addPackageListener();
        } else {
            StartService.getInstance().registerSdkCallback(TAG, mEngineObserver);
        }

//        AppCache.getInstance().getMContext().registerReceiver(new BroadcastReceiver() {
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
            //发送导航状态
            activelySendNavigationStatus();
        }

        @Override
        public void onSdkInitFail(final int code, final String msg) {
            Logger.e(FsaConstant.FSA_TAG, "engineInit error, code: " + code + ", msg: " + msg);
            sendEvent(FsaConstant.FsaFunction.ID_ROAD_NETWORK_MODE, FsaConstant.FsaValue.STRING_ZERO);
        }
    };

    public void activelySendNavigationStatus() {
        //发送导航状态
        judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_NAVIGATION, NaviStatus.NaviStatusType.NAVING);
        judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_LIGHT_NAVIGATION, NaviStatus.NaviStatusType.LIGHT_NAVING);
        judgeMapCurStatus(FsaConstant.FsaFunction.ID_IN_CRUISE, NaviStatus.NaviStatusType.CRUISE);
        sendCurrentNaviStatus(NaviStatusPackage.getInstance().getCurrentNaviStatus());
    }

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
     * 注册发送给MapService事件回调
     *
     * @param callBack ExportEventCallBack
     */
    public void registerExportEventCallBack(final ExportEventCallBack callBack) {
        if (mEventCallBack == null) {
            mEventCallBack = callBack;
        }
    }

    public void unregisterExportEventCallBack(final ExportEventCallBack callBack) {
        if (mEventCallBack == callBack) {
            mEventCallBack = null;
        }
    }

    /**
     * 根据收到的payload不同，发送对应的Event给客户端.
     *
     * @param functionId 客户端主动请求信息的事件ID，当前唯一9201.
     * @param payload    客户端事件所带的参数，区分请求内容.
     */
    @Override
    public void onReceiveRequest(final int functionId, final String payload) {
        Logger.d(FsaConstant.FSA_TAG, "onReceiveRequest payload==" , FsaIdString.event2String(payload) , " - functionId==" , functionId);
        if (FsaConstant.FsaMethod.ID_REQUEST_MSG == functionId) {
            Logger.i(FsaConstant.FSA_TAG, "received method request: payload = " , payload , " - " , FsaIdString.event2String(payload));
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
        Logger.d(FsaConstant.FSA_TAG, "----handlePayload1" , ThreadManager.getInstance().isMainThread());
        switch (payload) {
            case FsaConstant.FsaEventPayload.OPEN_HUD_MAP:
                updateMapDisplayStatus(true);
                ClusterMapOpenCloseManager.getInstance().triggerOpenOrClose(true);
                break;
            case FsaConstant.FsaEventPayload.CLOSE_HUD_MAP:
                updateMapDisplayStatus(false);
                ClusterMapOpenCloseManager.getInstance().triggerOpenOrClose(false);
                break;
            case FsaConstant.FsaEventPayload.OPEN_HUD_VIDEO:
                Logger.e(TAG, FsaIdString.event2String(payload),"mIsHudServiceStart: " + mIsHudServiceStart);
                if (!mIsHudServiceStart) startHud();
                break;
            case FsaConstant.FsaEventPayload.CLOSE_HUD_VIDEO:
                Logger.e(TAG, FsaIdString.event2String(payload),"mIsHudServiceStart: " + mIsHudServiceStart);
                if (mIsHudServiceStart) stopHud();
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
            case FsaConstant.FsaEventPayload.HUD_GONE:
                break;
            case FsaConstant.FsaEventPayload.HUD_LARGE:
                break;
            case FsaConstant.FsaEventPayload.HUD_BOTTOM_LEFT:
                break;
            case FsaConstant.FsaEventPayload.HUD_BOTTOM_RIGHT:
                break;
            case FsaConstant.FsaEventPayload.HUD_HINT:
                //属于CLEA平台 接收信号 去关闭HUD
                Logger.d(FsaConstant.FSA_TAG,"hud NO");
                ThreadManager.getInstance().postUi(() -> {
                    switchHudActivity(false);
                });
                break;
            case FsaConstant.FsaEventPayload.HUD_FULL:
                break;
            case FsaConstant.FsaEventPayload.HUD_LEFT_HALF:
                //属于CLEA平台 接收信号 去开启HUD
                Logger.d(FsaConstant.FSA_TAG,"hud OK");
                ThreadManager.getInstance().postUi(() -> {
                    switchHudActivity(true);
                });
                break;
            case FsaConstant.FsaEventPayload.HUD_RIGHT_HALF:
                break;
            default:
        }
    }

    //开启HUDMapviewActivity方法
    private void switchHudActivity(boolean isHud) {
        int secondeDid = 4; // HUD的DisplayId为4
        //if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && De)
        //cadi gb hud5
        //buick clea hud4
        if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_BUICK && CalibrationPackage.getInstance().architecture() == IS_CLEA){//buick
            Logger.d(FsaConstant.FSA_TAG, "switchHudActivity: yes IS_BUICK IS_CLEA");
            secondeDid = 4;
        }else if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_CADILLAC && CalibrationPackage.getInstance().architecture() == IS_GB){//Cadillac
            Logger.d(FsaConstant.FSA_TAG, "switchHudActivity: yes IS_CADILLAC IS_GB");
            secondeDid = 3;
        }else {
            Logger.d(FsaConstant.FSA_TAG, "switchHudActivity: NONONO");
            return;
        }
        Logger.d(FsaConstant.FSA_TAG, "switchHudActivity: ",isHud,secondeDid);
        if (isHud) {
            Logger.d(FsaConstant.FSA_TAG, "open HudActivity");
            final ActivityOptions options = ActivityOptions.makeBasic();
            options.setLaunchDisplayId(secondeDid);
            final Intent intent = new Intent();
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setAction("com.sgm.navi.hmi.hud.HudActivity");
            intent.putExtra("isHud", isHud);
            AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
        } else {
            //关闭HUD
            ThreadManager.getInstance().postUi(() -> {
                ActivityCloseManager.getInstance().triggerClose(false);});
        }
    }

    private void updateMapDisplayStatus(boolean isOpen) {
        try {
            Logger.d(TAG, "----switchClusterActivity();"+isOpen);
            handler.removeCallbacksAndMessages(null);
            if (isOpen) {
                handler.sendEmptyMessageDelayed(0,500);
            }else {
                handler.sendEmptyMessageDelayed(1,500);
            }
        } catch (Exception e) {
            Logger.e(TAG, "Failed to switch ClusterActivity state", e);
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
        SignalPackage.getInstance().registerObserver(FsaConstant.FSA_TAG, mSignalCallback);
        ThreeFingerFlyingScreenManager.getInstance().addOnCloseListener(this);
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
        if (functionId == FsaConstant.FsaFunction.ID_CHARGING_STATIONS_POI
                || functionId == FsaConstant.FsaFunction.ID_PARKING_LOT_POI
                || functionId == FsaConstant.FsaFunction.ID_SERVICE_POI
                || functionId == FsaConstant.FsaFunction.ID_GAS_STATION_POI) {
            if (!ConvertUtils.isEmpty(mEventCallBack)) {
                Logger.d(FsaConstant.FSA_TAG, "send event to mapService");
                mEventCallBack.onEventInform(functionId, info);
            }
        }

        final FsaServiceEvent event = (FsaServiceEvent) mService.eventHandler.getEventById(functionId);
        event.setOutputPayload(info.getBytes(StandardCharsets.UTF_8));
        if (Logger.isDebugLevel()) JsonLog.saveJsonToCache(info, "fsa.json", functionId + "-" + FsaIdString.function2String(functionId));
        if (functionId == FsaConstant.FsaFunction.ID_ENLARGE_ICON || functionId == FsaConstant.FsaFunction.ID_HUD_ENLARGE_MAP) {
            Logger.d(FsaConstant.FSA_TAG, "sendEvent: ",functionId, "-" , FsaIdString.function2String(functionId));
        } else if (functionId == FsaConstant.FsaFunction.ID_FINGER_FLYING_HUD) {//三指飞屏
            Logger.d(FsaConstant.FSA_TAG, "sendEvent: ",functionId, "-" ,FsaIdString.function2String(functionId));
        } else {
            Logger.d(FsaConstant.FSA_TAG, "sendEvent: ",functionId, "-" , FsaIdString.function2String(functionId) ," info = " , info);
        }
        mService.eventHandler.sendEvent(event);//, FSACatalog.DeviceName.UNKNOWN
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
        Logger.d(FsaConstant.FSA_TAG, "sendEventToMapCheckState: " , functionId , "-" , FsaIdString.function2String(functionId) , ", naviState = " , naviState);
        if (naviState.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            sendEventToMap(functionId, defaultValue);
        } else {
            sendEvent(functionId, defaultValue, false);
        }
    }

    @WorkerThread
    public void processMapCrossPicture(byte[] bytes) {
        if (!isShowCross) {
            Logger.d(TAG, "isShowImage==" , false);
            return;
        }
        long now = System.currentTimeMillis();
        if (now - lastProcessTime < MIN_INTERVAL) {
            return;
        }
        lastProcessTime = now;
        Bitmap orginBitmap = null;
        Bitmap flippedBitmap = null;
        Bitmap cropBitmap = null;
        try {
            int width = ScreenUtils.Companion.getInstance().getRealScreenWidth(AppCache.getInstance().getMContext());
            int height = ScreenUtils.Companion.getInstance().getRealScreenHeight(AppCache.getInstance().getMContext());
            Logger.d(TAG, "startScreenshot width==" , width , "height==" , height);
            orginBitmap = getBitmapFromPool(width, height);
            if (orginBitmap != null) {
                Logger.d(TAG, "processMapCrossPicture orginBitmap != null");
            }
            ByteBuffer buffer = ByteBuffer.wrap(bytes);
            Logger.d(TAG, "Bitmap size = " , orginBitmap.getByteCount());
            Logger.d(TAG, "Buffer size = " , buffer.capacity());
            //buffer.position(0);
            //buffer.rewind();
            orginBitmap.copyPixelsFromBuffer(buffer);
            Matrix matrix = new Matrix();
            matrix.postScale(1, -1);
            matrix.postTranslate(orginBitmap.getWidth(), orginBitmap.getHeight());
            flippedBitmap = Bitmap.createBitmap(orginBitmap, 0, 0,
                    orginBitmap.getWidth(), orginBitmap.getHeight(), matrix, true);
            Rect roadCrossRect = LayerAdapter.getInstance().getRoadCrossRect(MapType.MAIN_SCREEN_MAIN_MAP);
            cropBitmap = Bitmap.createBitmap(flippedBitmap, roadCrossRect.left, roadCrossRect.top, roadCrossRect.right - roadCrossRect.left, roadCrossRect.bottom - roadCrossRect.top);
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            cropBitmap.compress(Bitmap.CompressFormat.JPEG, 100, byteArrayOutputStream);
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            setCrossImg(byteArray);
        } catch (Exception e) {
            Logger.e(TAG, "Image processing error", e);
        } finally {
            safeRecycle(orginBitmap);
            safeRecycle(flippedBitmap);
            safeRecycle(cropBitmap);
        }
    }


    private Bitmap getBitmapFromPool(int width, int height) {
        synchronized (mLock) {
            for (Bitmap bitmap : mBitmapPool) {
                if (!bitmap.isRecycled() && bitmap.getWidth() == width && bitmap.getHeight() == height) {
                    mBitmapPool.remove(bitmap);
                    return bitmap;
                }
            }
            return Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
        }
    }

    private void safeRecycle(Bitmap bitmap) {
        if (bitmap != null && !bitmap.isRecycled()) {
            try {
                bitmap.recycle();
            } catch (Exception ignored) {
            }
        }
    }

//    @WorkerThread
//    public void processMapCrossPicture(byte[] bytes) {
//        Bitmap orginBitmap = null;
//        Bitmap flippedBitmap = null;
//        Bitmap cropBitmap;
//        try {
//            int width = ScreenUtils.Companion.getInstance().getScreenWidth();
//            int height = ScreenUtils.Companion.getInstance().getScreenHeight();
//            orginBitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
//            orginBitmap.copyPixelsFromBuffer(ByteBuffer.wrap(bytes));
//            // 翻转图像
//            Matrix matrix = new Matrix();
//            matrix.postScale(1, -1);
//            matrix.postTranslate(orginBitmap.getWidth(), orginBitmap.getHeight());
//            flippedBitmap = Bitmap.createBitmap(orginBitmap, 0, 0, orginBitmap.getWidth(), orginBitmap.getHeight(), matrix, true);
//            cropBitmap = Bitmap.createBitmap(flippedBitmap, 466, 174, 1106-466, 558-174);
//            final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
//            cropBitmap.compress(Bitmap.CompressFormat.JPEG, 100, byteArrayOutputStream);
//            byte[] byteArray = byteArrayOutputStream.toByteArray();
//            setCrossImg(byteArray);
//        } catch (Exception e) {
//            throw new RuntimeException(e);
//        } finally {
//            if (!ConvertUtils.isNull(orginBitmap)) {
//                orginBitmap.recycle();
//            }
//            if (!ConvertUtils.isNull(flippedBitmap)) {
//                flippedBitmap.recycle();
//            }
//        }
//    }

    //路口大图数据
    private byte[] crossImgRef;

    //设置路口大图数据
    public void setCrossImg(byte[] cropBitmap) {
        this.crossImgRef = cropBitmap;
    }

    //获取路口大图数据
    public byte[] getCrossImg() {
        return crossImgRef;
    }

    private IRouteResultObserver mRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onRouteResult(final RequestRouteResult requestRouteResult) {
            FsaNaviScene.getInstance().updateDestInfo(MyFsaService.this, requestRouteResult);
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
        if (CalibrationPackage.getInstance().architecture() == IS_CLEA){//CLEA平台 仪表的DisplayId=3
            secondeDid = 3;
        }
        if (isOpen) {
            Logger.d(FsaConstant.FSA_TAG, "----open ClusterActivity",secondeDid);
            final ActivityOptions options = ActivityOptions.makeBasic();
            options.setLaunchDisplayId(secondeDid);
            final Intent intent = new Intent();
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setAction("com.sgm.navi.hmi.cluster_map.ClusterActivity");
            intent.putExtra("isOpen", isOpen);
            AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
        } else {
            Logger.d(FsaConstant.FSA_TAG, "----close ClusterActivity");
            ActivityCloseManager.getInstance().triggerClose(true);
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
        Logger.d(TAG, "NativeInitialize ret is " , ret);
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
        mIsHudServiceStart = true;
        Logger.d(TAG, "service start NativeStart ret is " , ret, "mIsHudServiceStart: " , mIsHudServiceStart);
    }

    /**
     * 停止
     */
    public void stopHud() {
        Logger.d(TAG, "service stop");
        final int ret = VTServerBQJni.getInstance().nativeStop();
        mIsHudServiceStart = false;
        Logger.d(TAG, "service stop NativeStop ret is " , ret, "mIsHudServiceStart: " , mIsHudServiceStart);
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
        mIsHudServiceStart = false;
        Logger.d(TAG, "service uninit", "mIsHudServiceStart: " + mIsHudServiceStart);
        VTServerBQJni.getInstance().nativeUninitialize();
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
            ThreadManager.getInstance().execute(() -> {
                if (mChargeStationSF != null) {
                    mChargeStationSF.cancel(true);
                    mChargeStationSF = null;
                }
                if (mParkingLotSF != null) {
                    mParkingLotSF.cancel(true);
                    mParkingLotSF = null;
                }
                if (mServiceSF != null) {
                    mServiceSF.cancel(true);
                    mServiceSF = null;
                }
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
            });
        }
    };

    private final SignalCallback mSignalCallback = new SignalCallback() {
        @Override
        public void onFuelLevelPercentSignalChanged(float value) {
            if (value <= 15) {
                if (mGasStationSF != null) {
                    return;
                }
                mGasStationSF = ThreadManager.getInstance().asyncWithFixDelay(new Runnable() {
                    @Override
                    public void run() {
                        Logger.d(TAG, "搜索加油站");
                        mGasStationTaskId = SearchPackage.getInstance().aroundSearch(1, "加油站",
                                PositionPackage.getInstance().currentGeo, "2000", true);
                    }
                }, 0, 15, TimeUnit.SECONDS);
            } else {
                if (mGasStationSF != null) {
                    mGasStationSF.cancel(true);
                    mGasStationSF = null;
                }
            }
        }
    };

    /**
     * 搜索结果回调.
     */
    private final SearchResultCallback mSearchResultCallback = new SearchResultCallback() {

        @Override
        public void onSilentSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
            if (searchResultEntity == null) {
                return;
            }
            if (mChargeStationTaskId == taskId) {
                FsaNaviScene.getInstance().updateChargingStationInfo(MyFsaService.this, searchResultEntity);
            }
            if (mParkingLotTaskId == taskId) {
                FsaNaviScene.getInstance().updateParkingLotInfo(MyFsaService.this, searchResultEntity);
            }
            if (mGasStationTaskId == taskId) {
                FsaNaviScene.getInstance().updateGasStation(MyFsaService.this, searchResultEntity);
            }
        }
    };
    private boolean isShowCross = false;
    //引导信息
    private final IGuidanceObserver mGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviStart() {
            Logger.d(TAG, "导航开启 + mIsHudServiceStart：" , mIsHudServiceStart);
            if (!mIsHudServiceStart) return;
            HudPackage.getInstance().initHudService();
            HudPackage.getInstance().registerHudCallback(TAG, hudCallback);
        }

        @Override
        public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
            FsaNaviScene.getInstance().updateTbtInfo(MyFsaService.this, naviETAInfo);
            if (naviETAInfo != null && naviETAInfo.getRemainDist() <= 2000) {
                if (mParkingLotSF != null) {
                    return;
                }
                mParkingLotSF = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                    Logger.d(TAG, "搜索停车场");
                    RouteParam endPoint = RoutePackage.getInstance().getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
                    if (endPoint != null) {
                        mParkingLotTaskId = SearchPackage.getInstance().aroundSearch(1, "停车场",
                                endPoint.getRealPos(), "2000", true);
                    }
                }, 0, 15, TimeUnit.SECONDS);
            }
            if (getRemainDistance() < 50 * 1000) {
                String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
                if (Objects.equals(currentNaviStatus, NaviStatus.NaviStatusType.NAVING)) {
                    if (mChargeStationSF != null) {
                        return;
                    }
                    mChargeStationSF = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                        Logger.d(TAG, "搜索充电站");
                        mChargeStationTaskId = SearchPackage.getInstance().aroundSearch(1, "充电站",
                                PositionPackage.getInstance().currentGeo, "2000", true);
                    }, 0, 15, TimeUnit.SECONDS);
                }
            }
        }

        @Override
        public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
            FsaNaviScene.getInstance().updateLaneLineInfo(MyFsaService.this, isShowLane, laneInfoEntity);
        }

        @Override
        public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
            FsaNaviScene.getInstance().updateEnlargeMap(MyFsaService.this, isShowImage, naviImageInfo);
            isShowCross = isShowImage;
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
            if (sapaInfoEntity == null) {
                if (mServiceSF == null) {
                    return;
                }
                mServiceSF.cancel(true);
            } else {
                mSapaInfoEntity = sapaInfoEntity;
                if (mServiceSF != null) {
                    return;
                }
                mServiceSF = ThreadManager.getInstance().asyncWithFixDelay(new Runnable() {
                    @Override
                    public void run() {
                        FsaNaviScene.getInstance().updateHighwayService(MyFsaService.this, mSapaInfoEntity);
                    }
                }, 0, 15, TimeUnit.SECONDS);
            }
        }

        @Override
        public void onCurrentRoadSpeed(final int speed) {
            Logger.d(TAG, "当前道路限速：" , speed);
            FsaNaviScene.getInstance().updateSpeedLimitSignData(MyFsaService.this, speed);
        }

        @Override
        public void onUpdateTrafficLightCountdown(ArrayList<TrafficLightCountdownEntity> list) {
            if (list == null || list.isEmpty()) {
                return;
            }
            ArrayList<TrafficLightInfo> trafficLightInfoList = new ArrayList<>();
            for (TrafficLightCountdownEntity trafficLightCountdownEntity : list) {
                if (trafficLightCountdownEntity == null) {
                    continue;
                }
                TrafficLightInfo trafficLightInfo = new TrafficLightInfo();
                int trafficLightDis = L2Package.getInstance().getLinkDist(trafficLightCountdownEntity.getMSegmentIndex(), trafficLightCountdownEntity.getMLinkIndex());
                trafficLightInfo.setDistanceTrafficLight(trafficLightDis);
                LightInfoEntity lightInfoEntity = trafficLightCountdownEntity.getMLightInfo();
                if (lightInfoEntity == null) {
                    continue;
                }
                switch (lightInfoEntity.getMDir()) {
                    case 1:
                        trafficLightInfo.setTrafficLightAssignment(2);
                        break;
                    case 2:
                        trafficLightInfo.setTrafficLightAssignment(1);
                        break;
                    case 8:
                        trafficLightInfo.setTrafficLightAssignment(3);
                        break;
                    case 7:
                        trafficLightInfo.setTrafficLightAssignment(4);
                        break;
                    default:
                        trafficLightInfo.setTrafficLightAssignment(0);
                        break;
                }
                ArrayList<LightInfoEntity.LightStateEntity> lightStateEntities = lightInfoEntity.mLightStates;
                if (lightStateEntities == null || lightStateEntities.isEmpty()) {
                    continue;
                }
                LightInfoEntity.LightStateEntity lightStateEntity = lightStateEntities.get(0);
                if (lightStateEntity == null) {
                    continue;
                }
                switch (lightStateEntity.mLightType) {
                    case 2:
                        trafficLightInfo.setTrafficLightColor(1);
                        break;
                    case 3:
                        trafficLightInfo.setTrafficLightColor(3);
                        break;
                    case 4:
                        trafficLightInfo.setTrafficLightColor(2);
                        break;
                    default:
                        trafficLightInfo.setTrafficLightColor(0);
                        break;
                }
                trafficLightInfo.setTrafficLightTime((int) (System.currentTimeMillis() / 1000 - lightStateEntity.getMEtime()));
                trafficLightInfoList.add(trafficLightInfo);
            }
            String json = GsonUtils.toJson(trafficLightInfoList);
            sendEvent(FsaConstant.FsaFunction.ID_TRAFFIC_LIGHT_INFO, json);
        }

        @Override
        public void onNaviStop() {
            Logger.d(TAG, "导航退出 mIsHudServiceStart：" , mIsHudServiceStart);
            if (mIsHudServiceStart) HudPackage.getInstance().unInitHudService();
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
    };

    private final IHudCallback hudCallback = (mapTypeId, bytes) -> {
        if (bytes == null) {
            Logger.d(TAG, "onEGLScreenshot-->bytes==null");
            return;
        }
        if (mapTypeId == MapType.HUD_MAP) {//HUD路网图
            Logger.d(TAG, "onEGLScreenshot--> bytes: " , bytes.length);
            byte[] bytes1 = processPicture(bytes);
            VTServerBQJni.getInstance().nativeNotifyVideoData(bytes1);
        } else if (mapTypeId == MapType.MAIN_SCREEN_MAIN_MAP) {//主图的路口大图
            processMapCrossPicture(bytes);
        }
    };

    public interface ExportEventCallBack {
        void onEventInform(final int eventId, final String eventStr);
    }

    private int getRemainDistance() {
        return (int) (BevPowerCarUtils.getInstance().initlialHVBattenergy * BevPowerCarUtils.getInstance().batterToDistance);
    }
}
