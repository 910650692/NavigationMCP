package com.fy.navi;

import android.annotation.SuppressLint;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.IBinder;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.work.Constraints;
import androidx.work.Data;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.WorkInfo;
import androidx.work.WorkManager;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.android.utils.ConvertUtils;
import com.android.utils.file.ParseJsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.adas.AdasClient;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.code.CodeManager;
import com.fy.navi.service.define.code.ErrorCode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.aos.AosRestrictedPackage;
import com.fy.navi.service.logicpaket.hotupdate.HotUpdatePackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.recorder.RecorderPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
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
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.vrbridge.VrBridgeManager;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/1
 */
public class NaviService extends Service {
    private static final String TAG = MapDefaultFinalTag.INIT_SERVICE_TAG;
    private static final String NOTIFICATION_ID = "102";
    private static final String NOTIFICATION_NAME = "NaviService";
    private static final int FOREGROUND_SERVICE_ID = 0x02;
    public static final String START_APPLICATION_KEY = "startMapView";
    private final OneTimeWorkRequest engineWorkRequest;
    private final OneTimeWorkRequest parseJsonWorkRequest;
    private final OneTimeWorkRequest mapInitWorkRequest;
    private final OneTimeWorkRequest layerInitWorkRequest;
    private final OneTimeWorkRequest unInitEngineWorkRequest;
    private final OneTimeWorkRequest positionWorkRequest;
    private final OneTimeWorkRequest sdkServiceInitWorkRequest;
    private final OneTimeWorkRequest fsaInitWorkRequest;
    private final OneTimeWorkRequest vrBridgeWorkRequest;
    private SettingManager settingManager;
    private CommonManager commonManager;
    private static boolean isMapOnIniting = false;
    public static boolean isMapInited = false;
    private static final CopyOnWriteArrayList<INaviInitListener> initListeners = new CopyOnWriteArrayList<>();

    public NaviService() {
        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.NOT_REQUIRED) // TODO NetworkType.NOT_REQUIRED 为临时方案
                .build();
        parseJsonWorkRequest = new OneTimeWorkRequest.Builder(ParseJson.class)
                .addTag("ParseJson ErrorCode")
                .build();
        engineWorkRequest = new OneTimeWorkRequest.Builder(EngineInitWorker.class)
                .setConstraints(constraints)
                .addTag("Init Engine")
                .build();
        positionWorkRequest = new OneTimeWorkRequest.Builder(PositionInitWorker.class)
                .addTag("Init Position")
                .build();
        mapInitWorkRequest = new OneTimeWorkRequest.Builder(MapInit.class)
                .addTag("map Init")
                .build();
        layerInitWorkRequest = new OneTimeWorkRequest.Builder(LayerInit.class)
                .addTag("Layer Init")
                .build();
        sdkServiceInitWorkRequest = new OneTimeWorkRequest.Builder(SdkServiceInit.class)
                .addTag("SdkService Init")
                .build();
        unInitEngineWorkRequest = new OneTimeWorkRequest.Builder(EngineUnInitWorker.class)
                .addTag("UnInit Engine")
                .build();
        fsaInitWorkRequest = new OneTimeWorkRequest.Builder(FsaInitWorker.class)
                .addTag("Fsa Init")
                .build();
        vrBridgeWorkRequest = new OneTimeWorkRequest.Builder(VrBridgeInitWorker.class)
                .addTag("VrBridge init")
                .build();
    }

    @RequiresApi(api = Build.VERSION_CODES.UPSIDE_DOWN_CAKE)
    @Override
    public void onCreate() {
        super.onCreate();
        NotificationChannel channel = new NotificationChannel(NOTIFICATION_ID, NOTIFICATION_NAME,
                NotificationManager.IMPORTANCE_LOW);
        NotificationManager manager = (NotificationManager) this
                .getSystemService(Context.NOTIFICATION_SERVICE);
        manager.createNotificationChannel(channel);
        Notification notification = new Notification.Builder(this, NOTIFICATION_ID).build();
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
            startForeground(FOREGROUND_SERVICE_ID, notification);
        } else {
            startForeground(FOREGROUND_SERVICE_ID, notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_SHORT_SERVICE);
        }
        //初始化数据库
        settingManager = SettingManager.getInstance();
        commonManager = CommonManager.getInstance();
        settingManager.init();
        commonManager.init();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        ConvertUtils.isNullRequire(intent, "Please pass the interface startup parameters");
        boolean isStart = intent.getBooleanExtra(START_APPLICATION_KEY, false);
        Logger.i(TAG, "onStartCommand", "isStart:" + isStart);
        startInitEngine(isStart);
        return super.onStartCommand(intent, flags, startId);
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.i(TAG, "onDestroy");
    }

    private synchronized void startInitEngine(boolean isStart) {
        Logger.i(TAG, "startInitEngine", "isMapInited:" + isMapInited, "isMapOnIniting:" + isMapOnIniting);
        if (isMapInited) {
            Logger.i(TAG, "map had inited!");
            stopSelf();
            return;
        }
        if (isMapOnIniting) {
            Logger.i(TAG, "map on initing!");
            stopSelf();
            return;
        }
        isMapOnIniting = true;
        boolean hasActivity = StackManager.getInstance().getFirstActivity() != null;
        Logger.i(TAG, "hasActivity:" + hasActivity);
        //需要启动应用界面才会走此observer
        if (hasActivity) {
            ParseJson.addParseJsonObserver(parseJsonWorkRequest.getId());
            EngineInitWorker.addEngineObserver(engineWorkRequest.getId());
            PositionInitWorker.addPositionObserver(positionWorkRequest.getId());
            MapInit.addMapInitObserver(mapInitWorkRequest.getId());
            LayerInit.addLayerInitObserver(layerInitWorkRequest.getId());
            SdkServiceInit.addSdkServiceInitObserver(sdkServiceInitWorkRequest.getId(), isStart);
            FsaInitWorker.addEngineObserver(fsaInitWorkRequest.getId());
            VrBridgeInitWorker.addEngineObserver(vrBridgeWorkRequest.getId());
        }

        WorkManager.getInstance(AppContext.getInstance().getMContext())
                .beginWith(parseJsonWorkRequest)
                .then(engineWorkRequest)
                .then(positionWorkRequest)
                .then(mapInitWorkRequest)
                .then(layerInitWorkRequest)
                .then(sdkServiceInitWorkRequest)
                .then(fsaInitWorkRequest)
                .then(vrBridgeWorkRequest)
                .enqueue();
        stopSelf();
    }

    @SuppressLint("WorkerHasAPublicModifier")
    public static final class ParseJson extends Worker {
        public ParseJson(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
        }

        @NonNull
        @Override
        public Result doWork() {
            Logger.i(TAG, "parse doWork");
            String errCodePath = BuildConfig.MAP_SDK;
            String jsonPath = errCodePath + "/engine_code.json";
            String json = ParseJsonUtils.parseJsonFile(jsonPath);
            try {
                ErrorCode errorCode = CodeManager.getInstance().getErrorCode();
                JSONObject jsonObject = new JSONObject(json);
                parseErrorCode(errorCode, jsonObject);
                if (ConvertUtils.isEmpty(errorCode)) return Result.failure();
                return Result.success();
            } catch (JSONException e) {
                return Result.failure();
            }
        }

        private static void addParseJsonObserver(UUID uuid) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(uuid)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case ENQUEUED -> Logger.i(TAG, "parse json enqueued");
                            case RUNNING -> Logger.i(TAG, "parse json running");
                            case BLOCKED -> Logger.i(TAG, "parse json block");
                            case CANCELLED -> Logger.i(TAG, "parse json cancel");
                            case FAILED -> Logger.e(TAG, "parse json fail");
                            case SUCCEEDED -> Logger.i(TAG, "parse json success");
                        }
                    });
        }

        private void parseErrorCode(ErrorCode errorCode, JSONObject jsonObject) throws JSONException {
            String errorCodeJson = jsonObject.getJSONObject("error_code").toString();
            Map<Integer, String> mErrorCode = formJsonCode(errorCodeJson);
            errorCode.setErrorCode(mErrorCode);
            Logger.i(TAG, "Current mErrorCode: " + mErrorCode);
            String engineCodeJson = jsonObject.getJSONObject("engine_code").toString();
            Map<Integer, String> engineErrorCode = formJsonCode(engineCodeJson);
            errorCode.setEngineCode(engineErrorCode);
            Logger.i(TAG, "Current engineErrorCode: " + engineErrorCode);
            String mapCodeJson = jsonObject.getJSONObject("map_code").toString();
            Map<Integer, String> mapErrorCode = formJsonCode(mapCodeJson);
            errorCode.setMapCode(mapErrorCode);
            Logger.i(TAG, "Current mapErrorCode: " + mapErrorCode);
            String searchCodeJson = jsonObject.getJSONObject("search_code").toString();
            Map<Integer, String> searchErrorCode = formJsonCode(searchCodeJson);
            errorCode.setSearchCode(searchErrorCode);
            Logger.i(TAG, "Current searchErrorCode: " + searchErrorCode);
            String routeCodeJson = jsonObject.getJSONObject("route_code").toString();
            Map<Integer, String> routeErrorCode = formJsonCode(routeCodeJson);
            errorCode.setRouteCode(routeErrorCode);
            Logger.i(TAG, "Current routeCodeJson: " + routeCodeJson);
            String naviCodeJson = jsonObject.getJSONObject("navi_code").toString();
            Map<Integer, String> naviErrorCode = formJsonCode(naviCodeJson);
            errorCode.setNaviCode(naviErrorCode);
            Logger.i(TAG, "Current naviErrorCode: " + naviErrorCode);
            String settingCodeJson = jsonObject.getJSONObject("setting_code").toString();
            Map<Integer, String> settingErrorCode = formJsonCode(settingCodeJson);
            errorCode.setSettingCode(settingErrorCode);
            Logger.i(TAG, "Current settingErrorCode: " + settingErrorCode);
            String mapDataCodeJson = jsonObject.getJSONObject("map_data_code").toString();
            Map<Integer, String> mapDataErrorCode = formJsonCode(mapDataCodeJson);
            errorCode.setMapDataCode(mapDataErrorCode);
            Logger.i(TAG, "Current mapDataErrorCode: " + mapDataErrorCode);
            String accountCodeJson = jsonObject.getJSONObject("account_code").toString();
            Map<Integer, String> accountErrorCode = formJsonCode(accountCodeJson);
            errorCode.setAccountCode(accountErrorCode);
            Logger.i(TAG, "Current accountErrorCode: " + accountErrorCode);
            String forCastCodeJson = jsonObject.getJSONObject("for_cast_code").toString();
            Map<Integer, String> forCastErrorCode = formJsonCode(forCastCodeJson);
            errorCode.setForCastCode(forCastErrorCode);
            Logger.i(TAG, "Current forCastErrorCode: " + forCastErrorCode);
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
    }

    @SuppressLint("WorkerHasAPublicModifier")
    public static final class EngineInitWorker extends Worker {
        public EngineInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
        }

        @NonNull
        @Override
        public Result doWork() {
            Logger.i(TAG, "Engine doWork");
            return EnginePackage.getInstance().initEngine();
        }

        private static void addEngineObserver(UUID id) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(id)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case ENQUEUED -> Logger.i(TAG, "Engine init enqueued");
                            case RUNNING -> Logger.i(TAG, "Engine init running");
                            case BLOCKED -> Logger.d(TAG, "Engine init block");
                            case FAILED -> {
                                Data data = workInfo.getOutputData();
                                Logger.e(TAG, "Engine init fail", data.getKeyValueMap());
                            }
                            case CANCELLED -> Logger.i(TAG, "Engine init cancel");
                            case SUCCEEDED -> Logger.i(TAG, "Engine init success");
                        }
                    });
        }
    }

    public static final class PositionInitWorker extends Worker {
        public PositionInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
        }

        @NonNull
        @Override
        public Result doWork() {
            Logger.i(TAG, "Position doWork");
            PositionPackage.getInstance().init();
            return Result.success();
        }

        public static void addPositionObserver(UUID uuid) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(uuid)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case ENQUEUED -> Logger.i(TAG, "Position init enqueued");
                            case RUNNING -> Logger.i(TAG, "Position running");
                            case BLOCKED -> Logger.i(TAG, "Position block");
                            case FAILED -> Logger.e(TAG, "Position init fail");
                            case CANCELLED -> Logger.i(TAG, "Position cancel");
                            case SUCCEEDED -> {
                                Logger.i(TAG, "Position init success");
                                PositionPackage.getInstance().startPosition();
                            }
                        }
                    });
        }
    }

    public static final class MapInit extends Worker {
        public MapInit(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
        }

        @NonNull
        @Override
        public Result doWork() {
            Logger.i(TAG, "Map doWork");
            boolean mainMapInitResult = MapPackage.getInstance().init(MapType.MAIN_SCREEN_MAIN_MAP);
            boolean deskMapInitResult = MapPackage.getInstance().init(MapType.LAUNCHER_DESK_MAP);
            boolean widgetMapInitResult = MapPackage.getInstance().init(MapType.LAUNCHER_WIDGET_MAP);
            isMapInited = mainMapInitResult && deskMapInitResult && widgetMapInitResult;
            return (mainMapInitResult && deskMapInitResult && widgetMapInitResult) ? Result.success() : Result.failure();
        }

        private static void addMapInitObserver(UUID uuid) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(uuid)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case ENQUEUED -> Logger.i(TAG, "Map init enqueued");
                            case RUNNING -> Logger.i(TAG, "Map init running");
                            case BLOCKED -> Logger.i(TAG, "Map init block");
                            case FAILED -> Logger.e(TAG, "Map init fail");
                            case CANCELLED -> Logger.i(TAG, "Map init cancel");
                            case SUCCEEDED -> Logger.i(TAG, "Map init success");
                        }
                    });
        }
    }

    public static final class LayerInit extends Worker {
        public LayerInit(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
        }

        @NonNull
        @Override
        public Result doWork() {
            Logger.d(TAG, "LayerInit doWork");
            LayerPackage.getInstance().initLayerService();
            return Result.success();
        }

        private static void addLayerInitObserver(UUID uuid) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(uuid)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case ENQUEUED -> Logger.i(TAG, "NaviApp_Layer init enqueued");
                            case RUNNING -> Logger.i(TAG, "NaviApp_Layer init running");
                            case BLOCKED -> Logger.i(TAG, "NaviApp_Layer init block");
                            case FAILED -> Logger.e(TAG, "NaviApp_Layer init fail");
                            case CANCELLED -> Logger.i(TAG, "NaviApp_Layer init cancel");
                            case SUCCEEDED -> Logger.i(TAG, "NaviApp_Layer init success");
                        }
                    });
        }
    }

    public static final class SdkServiceInit extends Worker {

        public SdkServiceInit(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
        }

        @NonNull
        @Override
        public Result doWork() {
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
            return Result.success();
        }

        private static void addSdkServiceInitObserver(UUID uuid, boolean isStart) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(uuid)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case ENQUEUED -> Logger.i(TAG, "NaviApp_SdkService init enqueued");
                            case RUNNING -> Logger.i(TAG, "NaviApp_SdkService init running");
                            case BLOCKED -> Logger.i(TAG, "NaviApp_SdkService init block");
                            case FAILED -> {
                                Logger.e(TAG, "NaviApp_SdkService init fail");
                                notifyAppInitFinished(false);
                            }
                            case CANCELLED -> {
                                Logger.i(TAG, "NaviApp_SdkService init cancel");
                                notifyAppInitFinished(false);
                            }
                            case SUCCEEDED -> {
                                isMapInited = true;
                                Logger.i(TAG, "NaviApp_SdkService init success");
                                notifyAppInitFinished(true);
                            }
                        }
                    });
        }
    }

    public static final class EngineUnInitWorker extends Worker {
        public EngineUnInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);

        }

        @NonNull
        @Override
        public Result doWork() {
            MapPackage.getInstance().unInitMapService();
            LayerPackage.getInstance().unInitLayerService();
            RoutePackage.getInstance().unInitNaviService();
            NaviPackage.getInstance().unInitNaviService();
            CruisePackage.getInstance().unInitCruise();
            SearchPackage.getInstance().unInitSearchService();
            EnginePackage.getInstance().unInitEngine();
            return Result.success();
        }

        private static void addEngineObserver(UUID uuid) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(uuid)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case FAILED -> Logger.e(TAG, "Engine unInit fail");
                            case SUCCEEDED -> Logger.i(TAG, "Engine unInit success");
                        }
                    });
        }
    }

    /**
     * Fsa服务初始化.
     */
    public static final class FsaInitWorker extends Worker {

        public FsaInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
        }

        @NonNull
        @Override
        public Result doWork() {
            MyFsaService.getInstance().init();
            AdasClient.getInstance().start(AppContext.getInstance().getMContext());
            return Result.success();
        }

        private static void addEngineObserver(UUID uuid) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(uuid)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case BLOCKED -> Logger.i(TAG, "Fsa init block");
                            case FAILED -> Logger.e(TAG, "Fsa init fail");
                            case SUCCEEDED -> Logger.i(TAG, "Fsa init success");
                        }
                    });
        }
    }

    /**
     * 百度语音Bridge初始化
     */
    public static final class VrBridgeInitWorker extends Worker {

        public VrBridgeInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
        }

        @NonNull
        @Override
        public Result doWork() {
            ThreadManager.getInstance().postUi(() -> VrBridgeManager.getInstance().init(getApplicationContext()));
            return Result.success();
        }

        private static void addEngineObserver(UUID uuid) {
            WorkManager.getInstance(AppContext.getInstance().getMContext()).getWorkInfoByIdLiveData(uuid)
                    .observe(StackManager.getInstance().getFirstActivity(), workInfo -> {
                        WorkInfo.State state = workInfo.getState();
                        switch (state) {
                            case BLOCKED -> Logger.i(TAG, "VrBridge init block");
                            case FAILED -> Logger.e(TAG, "VrBridge init fail");
                            case SUCCEEDED -> Logger.i(TAG, "VrBridge init success");
                        }
                    });
        }

    }

    public static void registerAppInitListener(INaviInitListener listener) {
        Logger.i(TAG, "registerAppInitListener", "size:" + initListeners.size());
        if (!initListeners.contains(listener)) {
            Logger.i(TAG, "registerAppInitListener", "success");
            initListeners.add(listener);
        }
    }

    public static void unRegisterAppInitListener(INaviInitListener listener) {

        initListeners.remove(listener);
    }

    public static void notifyAppInitFinished(boolean result) {
        Logger.i(TAG, "notifyAppInitFinished", "size:" + initListeners.size());
        isMapOnIniting = false;
        initListeners.forEach((listener) -> {
            Logger.i(TAG, "notifyAppInitFinished");
            listener.onInitFinished(result);
        });
    }

    public static void exitProcess() {
        Logger.i(TAG, "exitProcess success!");
        System.exit(0);
    }
}