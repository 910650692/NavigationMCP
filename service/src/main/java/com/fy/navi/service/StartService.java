package com.fy.navi.service;

import com.android.utils.ConvertUtils;
import com.android.utils.file.ParseJsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.code.CodeManager;
import com.fy.navi.service.define.code.ErrorCode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.activate.ActivatePackage;
import com.fy.navi.service.logicpaket.activate.IActivateObserver;
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

import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/5/5
 */
public class StartService {
    private static final String TAG = MapDefaultFinalTag.INIT_SERVICE_TAG;
    private static int engineActive = -1;
    private static CopyOnWriteArrayList<ISdkInitCallback> sdkInitCallbacks; //此处回调必须使用List集合

    private StartService() {
        Logger.i(TAG, "start SDK before......");
        sdkInitCallbacks = new CopyOnWriteArrayList<>();
        EnginePackage.getInstance().addEngineObserver(TAG, engineObserver);
        ActivatePackage.getInstance().addActObserver(activateObserver);
        SettingManager.getInstance().init();
        CommonManager.getInstance().init();
    }

    public void registerSdkCallback(String key, ISdkInitCallback callback) {
        Logger.d(TAG, "call path name : " , key);
        ConvertUtils.push(sdkInitCallbacks, callback);
    }

    public void unregisterSdkCallback(ISdkInitCallback callback) {
        ConvertUtils.remove(sdkInitCallbacks, callback);
    }

    public void startInitSdk() {
        Logger.i(TAG, "start SDK......");
        startEngine();
    }

    /**
     * 引擎初始化失败需要重试，这个方法只能在NaviService中调用
     */
    public void retryEngineInit() {
        Logger.i(TAG, "retryEngineInit");
        startEngine();
    }

    /**
     * 获取引擎状态.
     *
     * @return -1：引擎没有初始化、0：引擎初始化成功、1、引擎初始化中
     */
    public int getSdkActivation() {
        Logger.i(TAG, "startEngine engineActive " , engineActive);
        return engineActive;
    }

    /**
     * 检测引擎状态
     *
     * @return true 需要重新初始化/false 不需要初始化
     */
    public boolean checkSdkIsNeedInit() {
        Logger.i(TAG, "checkSdkIsNeedInit engineActive " , getSdkActivation());
        if (-1 == engineActive) {
            Logger.i(TAG, "Engine not init");
            return true;
        } else if (1 == engineActive) {
            Logger.i(TAG, "Engine init progress......");
            conformInitializingCallback();
            return false;
        } else {
            Logger.i(TAG, "Engine already init finish");
            conformSuccessCallback();
            return false;
        }
    }

    private void startEngine() {
        Logger.i(TAG, "startEngine engineActive " , engineActive);
        if (1 == engineActive || 0 == engineActive) return;
        ThreadManager.getInstance().execute(() -> {
            conformInitializingCallback();
//            if (!parseErrorCode()) {
//                engineActive = -1;
//                Logger.e(TAG, "startEngine parseErrorCode failed engineActive = -1");
//                return;
//            }
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_CHANNEL_ID, EnginePackage.getInstance().getChanelName());
            EnginePackage.getInstance().initBaseLibs();
        });
    }

    private boolean parseErrorCode() {
        try {
            Logger.i(TAG, "parse doWork");
            ErrorCode errorCode = CodeManager.getInstance().getErrorCode();
            if (!ConvertUtils.isNull(errorCode)) return true;
            String errCodePath = BuildConfig.MAP_SDK;
            String jsonPath = errCodePath + "/engine_code.json";
            String json = ParseJsonUtils.parseJsonFile(jsonPath);
            JSONObject jsonObject = new JSONObject(json);
            parseErrorCode(errorCode, jsonObject);
            if (!ConvertUtils.isEmpty(errorCode)) {
                Logger.i(TAG, "errorCode parse success");
                return true;
            }

        } catch (Exception e) {
            Logger.i(TAG, "Exception");
            return false;
        }
        return false;
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
        Logger.i(TAG, "initSdkService");
        initPositionService();
        initMapView();
        if (!initLayerService()) {
            conformFailCallback(40001, "图层服务初始化失败");
            return;
        }
        initOtherService();
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
        MapPackage.getInstance().initMapService();
    }

    private boolean initLayerService() {
        Logger.d(TAG, "initLayerService");
        boolean initLayerResult = LayerPackage.getInstance().initLayerService();
        if (initLayerResult) {
            Logger.i(TAG, "Layer service init success");
        } else {
            Logger.i(TAG, "Layer service init fail");
        }
        return initLayerResult;
    }

    private void initOtherService() {
        Logger.i(TAG, "initOtherService start");
        SearchPackage.getInstance().initSearchService();
        NaviPackage.getInstance().initNaviService();
        SettingPackage.getInstance().init();
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
        SignalPackage.getInstance().init(AppCache.getInstance().getMContext());
        SpeechPackage.getInstance().init();
        NaviAudioPlayer.getInstance().init();
        HotUpdatePackage.getInstance().initService();
        Logger.i(TAG, "initOtherService end");
    }

    /**
     * 开启激活流程
     */
    private void startActivation() {
        Logger.i(TAG, "Sdk no activation, Start activation in progress.....");
        ActivatePackage.getInstance().startActivate();
    }

    /**
     * 整合引擎初始化中的回调
     */
    private void conformInitializingCallback() {
        Logger.i(TAG, "Sdk init progress......", "sdkInitCallbacks : " , sdkInitCallbacks.size());
        engineActive = 1;
        for (ISdkInitCallback callback : sdkInitCallbacks) {
            callback.onSdkInitializing();
        }
    }

    /**
     * 整合初始化成功的回调
     */
    private void conformSuccessCallback() {
        Logger.i(TAG, "Sdk init success", "sdkInitCallbacks : " , sdkInitCallbacks.size());
        engineActive = 0;
        ThreadManager.getInstance().postUi(() -> {
            for (ISdkInitCallback callback : sdkInitCallbacks) {
                callback.onSdkInitSuccess();
            }
        });
    }

    /**
     * 整合初始化失败的回调
     *
     * @param initSdkResult ErrorCode
     * @param msg           Error msg
     */
    private void conformFailCallback(int initSdkResult, String msg) {
        Logger.i(TAG, "Sdk init fail", "sdkInitCallbacks : " , sdkInitCallbacks.size());
        engineActive = -1;
        for (ISdkInitCallback callback : sdkInitCallbacks) {
            callback.onSdkInitFail(initSdkResult, msg);
        }
    }

    /**
     * 反初始化引擎
     */
    public void unSdkInit() {
        EnginePackage.getInstance().removeEngineObserver(TAG);
        MapPackage.getInstance().unInitMapService();
        LayerPackage.getInstance().unInitLayerService();
        RoutePackage.getInstance().unInitNaviService();
        NaviPackage.getInstance().unInitNaviService();
        CruisePackage.getInstance().unInitCruise();
        SearchPackage.getInstance().unInitSearchService();
        EnginePackage.getInstance().unInitEngine();
        ActivatePackage.getInstance().unInit();
        engineActive = -1;
    }

    private void parseErrorCode(ErrorCode errorCode, JSONObject jsonObject) throws JSONException {
        String errorCodeJson = jsonObject.getJSONObject("error_code").toString();
        Map<Integer, String> mErrorCode = formJsonCode(errorCodeJson);
        errorCode.setErrorCode(mErrorCode);
        Logger.i(TAG, "Current mErrorCode: " , mErrorCode);
        String engineCodeJson = jsonObject.getJSONObject("engine_code").toString();
        Map<Integer, String> engineErrorCode = formJsonCode(engineCodeJson);
        errorCode.setEngineCode(engineErrorCode);
        Logger.i(TAG, "Current engineErrorCode: " , engineErrorCode);

        String activationCode = jsonObject.getJSONObject("activation_code").toString();
        Map<Integer, String> activationErrorCode = formJsonCode(activationCode);
        errorCode.setActivateCode(activationErrorCode);
        Logger.i(TAG, "Current activationErrorCode: " , activationErrorCode);

        String positionCode = jsonObject.getJSONObject("position_code").toString();
        Map<Integer, String> positionErrorCode = formJsonCode(positionCode);
        errorCode.setPositionCode(positionErrorCode);
        Logger.i(TAG, "Current positionErrorCode: " , positionErrorCode);

        String mapCodeJson = jsonObject.getJSONObject("map_code").toString();
        Map<Integer, String> mapErrorCode = formJsonCode(mapCodeJson);
        errorCode.setMapCode(mapErrorCode);
        Logger.i(TAG, "Current mapErrorCode: " , mapErrorCode);

        String layerCodeJson = jsonObject.getJSONObject("layer_code").toString();
        Map<Integer, String> layerErrorCode = formJsonCode(layerCodeJson);
        errorCode.setLayerCode(layerErrorCode);
        Logger.i(TAG, "Current layerErrorCode: " , layerErrorCode);

        String searchCodeJson = jsonObject.getJSONObject("search_code").toString();
        Map<Integer, String> searchErrorCode = formJsonCode(searchCodeJson);
        errorCode.setSearchCode(searchErrorCode);
        Logger.i(TAG, "Current searchErrorCode: " , searchErrorCode);

        String routeCodeJson = jsonObject.getJSONObject("route_code").toString();
        Map<Integer, String> routeErrorCode = formJsonCode(routeCodeJson);
        errorCode.setRouteCode(routeErrorCode);
        Logger.i(TAG, "Current routeCodeJson: " , routeCodeJson);

        String naviCodeJson = jsonObject.getJSONObject("navi_code").toString();
        Map<Integer, String> naviErrorCode = formJsonCode(naviCodeJson);
        errorCode.setNaviCode(naviErrorCode);
        Logger.i(TAG, "Current naviErrorCode: " , naviErrorCode);

        String settingCodeJson = jsonObject.getJSONObject("setting_code").toString();
        Map<Integer, String> settingErrorCode = formJsonCode(settingCodeJson);
        errorCode.setSettingCode(settingErrorCode);
        Logger.i(TAG, "Current settingErrorCode: " , settingErrorCode);

        String mapDataCodeJson = jsonObject.getJSONObject("map_data_code").toString();
        Map<Integer, String> mapDataErrorCode = formJsonCode(mapDataCodeJson);
        errorCode.setMapDataCode(mapDataErrorCode);
        Logger.i(TAG, "Current mapDataErrorCode: " , mapDataErrorCode);

        String accountCodeJson = jsonObject.getJSONObject("account_code").toString();
        Map<Integer, String> accountErrorCode = formJsonCode(accountCodeJson);
        errorCode.setAccountCode(accountErrorCode);
        Logger.i(TAG, "Current accountErrorCode: " , accountErrorCode);

        String forCastCodeJson = jsonObject.getJSONObject("for_cast_code").toString();
        Map<Integer, String> forCastErrorCode = formJsonCode(forCastCodeJson);
        errorCode.setForCastCode(forCastErrorCode);
        Logger.i(TAG, "Current forCastErrorCode: " , forCastErrorCode);
    }

    private Map<Integer, String> formJsonCode(String json) throws JSONException {
        Map<Integer, String> errorCode = new HashMap<>();
        JSONObject engineCodeObject = new JSONObject(json);
        for (Iterator<String> it = engineCodeObject.keys(); it.hasNext(); ) {
            String key = it.next();
            Integer keyCode = ConvertUtils.str2Int(key);
            String keyMsg = engineCodeObject.get(key).toString();
            errorCode.put(keyCode, keyMsg);
        }
        return errorCode;
    }

    public static StartService getInstance() {
        return Helper.sts;
    }

    private static final class Helper {
        private final static StartService sts = new StartService();
    }

    private static final IActivateObserver activateObserver = new IActivateObserver() {
        @Override
        public void onActivating() {
        }

        @Override
        public void onNetActivateFailed(final int failedCount) {
        }

        @Override
        public void onActivated() {
            Logger.d(TAG, "onActivated in startService...");
            getInstance().checkActivation();
        }

        @Override
        public void onActivatedError(final int errCode, final String msg) {
        }
    };

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
            Logger.i(TAG, "Engine init fail", "errorCode：" , code, "errorMsg：" , msg);
            getInstance().conformFailCallback(code, msg);
        }
    };

    public interface ISdkInitCallback {
        default void onSdkInitializing() {

        }

        default void onSdkInitSuccess() {

        }

        default void onSdkInitFail(int initSdkResult, String msg) {

        }
    }
}
