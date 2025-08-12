package com.sgm.navi;

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
import androidx.work.OneTimeWorkRequest;
import androidx.work.WorkManager;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.android.utils.SpUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.adas.AdasClient;
import com.sgm.navi.flavor.BaseCarModelsFeature;
import com.sgm.navi.flavor.CarModelsFeature;
import com.sgm.navi.hmi.activate.ActivateUiStateManager;
import com.sgm.navi.hmi.launcher.LauncherWindowService;
import com.sgm.navi.hmi.splitscreen.SRFloatWindowService;
import com.sgm.navi.l2pp.PatacL2ppManager;
import com.sgm.navi.fsa.MyFsaService;
import com.sgm.navi.navisender.NaviSender;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.logicpaket.l2.L2Package;
import com.sgm.navi.vrbridge.VrBridgeManager;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/1
 */
public class NaviService extends Service {

    private static final String TAG = MapDefaultFinalTag.INIT_HMI_TAG;
    private static final String NOTIFICATION_ID = "102";
    private static final String NOTIFICATION_NAME = "NaviService";
    private static final int FOREGROUND_SERVICE_ID = 0x02;
    public static final String START_APPLICATION_KEY = "startMapView";
    private static OneTimeWorkRequest fsaInitWorkRequest;
    private static OneTimeWorkRequest vrBridgeWorkRequest;
    private static OneTimeWorkRequest carModelsFeatureWorkRequest;
    private static final AtomicBoolean atomicBoolean = new AtomicBoolean(false);
    private static boolean initSdkLogSwitch = false;

    public NaviService() {
        StartService.getInstance().registerSdkCallback(TAG, sdkInitCallback);
        fsaInitWorkRequest = new OneTimeWorkRequest.Builder(FsaInitWorker.class)
                .addTag("Fsa Init")
                .build();
        vrBridgeWorkRequest = new OneTimeWorkRequest.Builder(VrBridgeInitWorker.class)
                .addTag("VrBridge init")
                .build();
        carModelsFeatureWorkRequest = new OneTimeWorkRequest.Builder(CarModelsFeatureWork.class)
                .addTag("VrBridge init")
                .build();
        initSdkLogSwitch = SpUtils.getInstance().getBoolean(SpUtils.SP_KEY_LOG_SWITCH, false);
        startInitWhenApplicationCreate();
    }

    private void startInitWhenApplicationCreate() {
        ThreadManager.getInstance().execute(() -> {
            LauncherWindowService.startService();
        });
    }

    @RequiresApi(api = Build.VERSION_CODES.UPSIDE_DOWN_CAKE)
    @Override
    public void onCreate() {
        super.onCreate();
        Logger.e(TAG, "onCreate");
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
        ThreadManager.getInstance().execute(() -> {
            boolean sdkStatus = StartService.getInstance().checkSdkIsNeedInit();
            Logger.e(TAG, "校验Sdk是否需要初始化sdkStatus：", sdkStatus);
            if (sdkStatus) StartService.getInstance().startInitSdk();
        });
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Logger.e(TAG, "onStartCommand");
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
        Logger.e(TAG, "onDestroy");
    }

    /**
     * Fsa服务初始化.
     */
    public static final class FsaInitWorker extends Worker {

        public FsaInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
            Logger.d(TAG, "FsaInitWorker constructor");
        }

        @NonNull
        @Override
        public Result doWork() {
            Logger.d(TAG, "FsaInitWorker doWork");
            MyFsaService.getInstance().init();
            L2Package.getInstance().init();
            AdasClient.getInstance().init(AppCache.getInstance().getMContext());
            PatacL2ppManager.getInstance().init();
            NaviSender.getInstance().start();
            return Result.success();
        }
    }

    /**
     * 百度语音Bridge初始化
     */
    public static final class VrBridgeInitWorker extends Worker {

        public VrBridgeInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
            Logger.e(TAG, "VrBridgeInitWorker constructor");
        }

        @NonNull
        @Override
        public Result doWork() {
            ThreadManager.getInstance().postUi(() -> VrBridgeManager.getInstance().init(getApplicationContext()));
            return Result.success();
        }
    }

    public static final class CarModelsFeatureWork extends Worker {
        final BaseCarModelsFeature carModelsFeature;

        public CarModelsFeatureWork(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
            Logger.e(TAG, "CarModelsFeatureWork constructor");
            carModelsFeature = CarModelsFeature.getInstance();
        }

        @NonNull
        @Override
        public Result doWork() {
            carModelsFeature.initComponent();
            return Result.success();
        }
    }

    private static final StartService.ISdkInitCallback sdkInitCallback = new StartService.ISdkInitCallback() {
        @Override
        public void onSdkInitSuccess() {
            StartService.getInstance().unregisterSdkCallback(TAG, sdkInitCallback);
            WorkManager.getInstance(AppCache.getInstance().getMContext())
                    .beginWith(fsaInitWorkRequest)
                    .then(vrBridgeWorkRequest)
                    .then(carModelsFeatureWorkRequest)
                    .enqueue();
        }

        @Override
        public void onSdkBaseLibInitSuccess() {
            Logger.e(TAG, "onSdkBaseLibInitSuccess");
            ActivateUiStateManager.getInstance().init();
        }

        @Override
        public void onSdkInitFail(int initSdkResult, String msg) {
            if (!atomicBoolean.get()) {
                Logger.e(TAG, "Engine init fail and the retry in progress......");
                StartService.getInstance().retryEngineInit();
                // 在log没有打开的情况下，如果引擎初始化失败，则强制打开log开关，方便排查问题
                atomicBoolean.set(true);
                if (initSdkLogSwitch) return;
                initSdkLogSwitch = true;
                Logger.switchLog(true);
            } else {
                Logger.e(TAG, "Engine init fail and the number of retries exceeded");
            }
        }
    };
}