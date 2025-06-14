package com.fy.navi;

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

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.adas.AdasClient;
import com.fy.navi.broadcast.SteeringWheelButtonReceiver;
import com.fy.navi.l2pp.PatacL2ppManager;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.navisender.NaviSender;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.StartService;
import com.fy.navi.vrbridge.VrBridgeManager;

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
        StartService.getInstance().registerSdkCallback(TAG, sdkInitCallback);
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
        Logger.i(TAG, "onCreate");
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
        Logger.i(TAG, "onStartCommand");
        boolean sdkStatus = StartService.getInstance().checkSdkIsNeedInit();
        Logger.i(TAG, "校验Sdk是否需要初始化sdkStatus：" + sdkStatus);
        if (sdkStatus) StartService.getInstance().startInitSdk();
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
        AppCache.getInstance().getMContext().registerReceiver(steeringWheelButtonReceiver, intentFilter, Context.RECEIVER_EXPORTED);
    }

    /**
     * Fsa服务初始化.
     */
    public static final class FsaInitWorker extends Worker {

        public FsaInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
            Logger.i(TAG, "FsaInitWorker constructor");
        }

        @NonNull
        @Override
        public Result doWork() {
            Logger.d(TAG, "FsaInitWorker doWork");
            MyFsaService.getInstance().init();
            AdasClient.getInstance().init(AppCache.getInstance().getMContext());
            PatacL2ppManager.getInstance().init();
            NaviSender.getInstance().init();
            return Result.success();
        }
    }

    /**
     * 百度语音Bridge初始化
     */
    public static final class VrBridgeInitWorker extends Worker {

        public VrBridgeInitWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
            super(context, workerParams);
            Logger.i(TAG, "VrBridgeInitWorker constructor");
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
            WorkManager.getInstance(AppCache.getInstance().getMContext())
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