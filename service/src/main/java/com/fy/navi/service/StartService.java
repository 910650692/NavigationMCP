package com.fy.navi.service;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.activate.ActivatePackage;
import com.fy.navi.service.logicpaket.aos.AosRestrictedPackage;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.engine.IEngineObserver;
import com.fy.navi.service.logicpaket.hotupdate.HotUpdatePackage;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.recorder.RecorderPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import com.fy.navi.service.logicpaket.speech.SpeechPackage;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.service.logicpaket.user.carconnect.CarConnectPackage;
import com.fy.navi.service.logicpaket.user.forecast.ForecastPackage;
import com.fy.navi.service.logicpaket.user.group.GroupPackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.service.logicpaket.voice.VoicePackage;
import com.fy.navi.service.tts.NaviAudioPlayer;

import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/5/5
 */
public class StartService {
    private static final String TAG = MapDefaultFinalTag.START_SDK_TAG;
    private static boolean engineActive;
    private static CopyOnWriteArrayList<ISdkInitCallback> sdkInitCallbacks; //此处回调必须使用List集合

    private StartService() {
        Logger.i(TAG, "start SDK before......");
        sdkInitCallbacks = new CopyOnWriteArrayList<>();
        EnginePackage.getInstance().addEngineObserver(TAG, engineObserver);
        SettingManager.getInstance().init();
        CommonManager.getInstance().init();
    }

    public void registerSdkCallback(ISdkInitCallback callback) {
        ConvertUtils.push(sdkInitCallbacks, callback);
    }

    public void unregisterSdkCallback(ISdkInitCallback callback) {
        ConvertUtils.remove(sdkInitCallbacks, callback);
    }

    public void startInitSdk() {
        Logger.i(TAG, "start SDK......");
        startEngine();
    }

    public void retryEngineInit() {
        startEngine();
    }

    public boolean getSdkActivityStatus() {
        return engineActive;
    }

    private void startEngine() {
        ThreadManager.getInstance().execute(() -> {
            if (checkSdkActivation()) return;
            Logger.i(TAG, "Start engine in progress.....");
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_CHANNEL_ID, EnginePackage.getInstance().getChanelName());
            EnginePackage.getInstance().initBaseLibs();
        });
    }

    public boolean checkSdkActivation() {
        if (engineActive) {
            Logger.i(TAG, "Engine already init finish");
            conformSuccessCallback();
        }
        return engineActive;
    }

    /**
     * 激活校验
     */
    private void checkActivation() {
        if (ActivatePackage.getInstance().checkActivation()) {
            Logger.i(TAG, "Sdk already activation");
            EnginePackage.getInstance().initBL();
        } else {
            startActivation();
        }
    }

    /**
     * 初始化SDK，注意时序不能变
     */
    private void initSdkService() {
        initPositionService();
        initMapView();
        if (!initLayerService()) {
            conformFailCallback(40001, "图层服务初始化失败");
            return;
        }
        initOtherService();
        engineActive = true;
        conformSuccessCallback();
    }

    private void initPositionService() {
        boolean iniPositionResult = PositionPackage.getInstance().init();
        if (!iniPositionResult) {
            // 理论上定位服务初始化失败不能影响后边逻辑，没有定位只是位置显示不正确
            Logger.i(TAG, "Position service init fail");
            conformFailCallback(30002, "定位服务初始化失败"); // 需要从json中读取，暂时先写死
        } else {
            Logger.i(TAG, "Position service init success");
            PositionPackage.getInstance().startPosition();
        }
    }

    private void initMapView() {
        MapPackage.getInstance().init(MapType.MAIN_SCREEN_MAIN_MAP);
        MapPackage.getInstance().init(MapType.LAUNCHER_DESK_MAP);
        MapPackage.getInstance().init(MapType.LAUNCHER_WIDGET_MAP);
        MapPackage.getInstance().init(MapType.HUD_MAP);
    }

    private boolean initLayerService() {
        boolean initLayerResult = LayerPackage.getInstance().initLayerService();
        if (initLayerResult) {
            Logger.i(TAG, "Layer service init success");
        } else {
            Logger.i(TAG, "Layer service init fail");
        }
        return initLayerResult;
    }

    private void initOtherService() {
        SettingPackage.getInstance().init();
        SearchPackage.getInstance().initSearchService();
        NaviPackage.getInstance().initNaviService();
        RoutePackage.getInstance().initRouteService();
        CruisePackage.getInstance().initCruise();
        MapDataPackage.getInstance().initMapDataService();
        RecorderPackage.getInstance().initService();
        UserTrackPackage.getInstance().initUserTrackService();
        AccountPackage.getInstance().initAccountService();
        BehaviorPackage.getInstance().initBehaviorService();
        CarConnectPackage.getInstance().initService();
        ForecastPackage.getInstance().initService();
        GroupPackage.getInstance().initService();
        MsgPushPackage.getInstance().initService();
        AosRestrictedPackage.getInstance().initAosService();
        VoicePackage.getInstance().init();
        SignalPackage.getInstance().init(AppContext.getInstance().getMContext());
        SpeechPackage.getInstance().init();
        NaviAudioPlayer.getInstance().init();
        HotUpdatePackage.getInstance().initService();
    }

    /**
     * 开启激活流程
     */
    private void startActivation() {
        Logger.i(TAG, "Sdk no activation, Start activation in progress.....");
    }

    private void conformSuccessCallback() {
        Logger.i(TAG, "Sdk init success", "sdkInitCallbacks : " + sdkInitCallbacks.size());
        ThreadManager.getInstance().postUi(() -> {
            for (ISdkInitCallback callback : sdkInitCallbacks) {
                callback.onSdkInitSuccess();
            }
        });
    }

    private void conformFailCallback(int initSdkResult, String msg) {
        Logger.i(TAG, "Sdk init fail");
        for (ISdkInitCallback callback : sdkInitCallbacks) {
            callback.onSdkInitFail(initSdkResult, msg);
        }
    }

    public void unSdkInit() {
        EnginePackage.getInstance().removeEngineObserver(TAG);
        MapPackage.getInstance().unInitMapService();
        LayerPackage.getInstance().unInitLayerService();
        RoutePackage.getInstance().unInitNaviService();
        NaviPackage.getInstance().unInitNaviService();
        CruisePackage.getInstance().unInitCruise();
        SearchPackage.getInstance().unInitSearchService();
        EnginePackage.getInstance().unInitEngine();
    }

    public static StartService getInstance() {
        return Helper.sts;
    }

    private static final class Helper {
        private final static StartService sts = new StartService();
    }

    private static final IEngineObserver engineObserver = new IEngineObserver() {
        @Override
        public void onInitBaseLibSuccess() {
            Logger.i(TAG, "Engine baseLib init success");
            getInstance().checkActivation();
        }

        @Override
        public void onInitEngineSuccess() {
            Logger.i(TAG, "Engine init success.....");
            getInstance().initSdkService();
        }

        @Override
        public void onInitEngineFail(int code, String msg) {
            Logger.i(TAG, "Engine init fail", "errorCode：" + code, "errorMsg：" + msg);
            getInstance().conformFailCallback(code, msg);
        }
    };

    public interface ISdkInitCallback {
        default void onSdkInitSuccess() {

        }

        default void onSdkInitFail(int initSdkResult, String msg){

        }
    }
}
