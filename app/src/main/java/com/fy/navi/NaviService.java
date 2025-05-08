package com.fy.navi;

import android.annotation.SuppressLint;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.IBinder;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.work.ListenableWorker;
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
import com.fy.navi.broadcast.SteeringWheelButtonReceiver;
import com.fy.navi.clslink.ClsLinkManager;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.StartService;
import com.fy.navi.service.define.code.CodeManager;
import com.fy.navi.service.define.code.ErrorCode;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.vrbridge.VrBridgeManager;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

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
    private static OneTimeWorkRequest fsaInitWorkRequest;
    private static OneTimeWorkRequest vrBridgeWorkRequest;
    private static AtomicBoolean atomicBoolean = new AtomicBoolean(false);

    public NaviService() {
        initBroadcast();
        StartService.getInstance().registerSdkCallback(sdkInitCallback);
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
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Logger.i(TAG, "校验Sdk的可用状态");
        boolean sdkStatus = StartService.getInstance().checkSdkActivation();
        if (!sdkStatus) {
            Logger.i(TAG, "Sdk尚未被初始化");
            startParseJson();
        }else {
            Logger.i(TAG, "Sdk已经初始化");
        }
        stopSelf();
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

    private void initBroadcast() {
        SteeringWheelButtonReceiver steeringWheelButtonReceiver = new SteeringWheelButtonReceiver();
        IntentFilter intentFilter = new IntentFilter(SteeringWheelButtonReceiver.ACTION);
        AppContext.getInstance().getMContext().registerReceiver(steeringWheelButtonReceiver, intentFilter, Context.RECEIVER_EXPORTED);
    }

    private synchronized void startParseJson() {
        ThreadManager.getInstance().execute(() -> {
            Logger.i(TAG, "parse doWork");
            String errCodePath = BuildConfig.MAP_SDK;
            String jsonPath = errCodePath + "/engine_code.json";
            String json = ParseJsonUtils.parseJsonFile(jsonPath);
            try {
                ErrorCode errorCode = CodeManager.getInstance().getErrorCode();
                JSONObject jsonObject = new JSONObject(json);
                parseErrorCode(errorCode, jsonObject);
                if (ConvertUtils.isEmpty(errorCode)) {
                    Logger.i(TAG, "errorCode empty");
                }
            } catch (JSONException e) {
                Logger.i(TAG, "Exception");
            }

            StartService.getInstance().startInitSdk();
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

        String activationCode = jsonObject.getJSONObject("activation_code").toString();
        Map<Integer, String> activityErrorCode = formJsonCode(activationCode);
        errorCode.setMapCode(activityErrorCode);
        Logger.i(TAG, "Current activityErrorCode: " + activityErrorCode);

        String positionCode = jsonObject.getJSONObject("position_code").toString();
        Map<Integer, String> positionErrorCode = formJsonCode(positionCode);
        errorCode.setMapCode(positionErrorCode);
        Logger.i(TAG, "Current positionErrorCode: " + positionErrorCode);

        String mapCodeJson = jsonObject.getJSONObject("map_code").toString();
        Map<Integer, String> mapErrorCode = formJsonCode(mapCodeJson);
        errorCode.setMapCode(mapErrorCode);
        Logger.i(TAG, "Current mapErrorCode: " + mapErrorCode);

        String layerCodeJson = jsonObject.getJSONObject("layer_code").toString();
        Map<Integer, String> layerErrorCode = formJsonCode(layerCodeJson);
        errorCode.setMapCode(layerErrorCode);
        Logger.i(TAG, "Current layerErrorCode: " + layerErrorCode);

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
            ClsLinkManager.getInstance().init();
            return Result.success();
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
    }

    private static final StartService.ISdkInitCallback sdkInitCallback = new StartService.ISdkInitCallback() {
        @Override
        public void onSdkInitSuccess() {
            WorkManager.getInstance(AppContext.getInstance().getMContext())
                    .beginWith(fsaInitWorkRequest)
                    .then(vrBridgeWorkRequest)
                    .enqueue();
        }

        @Override
        public void onSdkInitFail(int initSdkResult, String msg) {
            if (!atomicBoolean.get()) {
                Logger.i(TAG, "Engine init fail and the retry in progress......");
                StartService.getInstance().retryEngineInit();
                atomicBoolean.set(true);
            } else {
                Logger.i(TAG, "Engine init fail and the number of retries exceeded");
            }
        }
    };
}