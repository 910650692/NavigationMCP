package com.fy.navi.exportservice;

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
import android.util.Log;

import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;

public class MapService extends Service {
    private static final String TAG = MapService.class.getSimpleName();

    private static final String NOTIFICATION_ID = "101";
    private static final String NOTIFICATION_NAME = "MapService";
    private static final int FOREGROUND_SERVICE_ID = 1;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        String packageName = intent.getStringExtra("packageName");
        Log.d(TAG, "onBind: " + packageName);
        return BinderPool.getInstance();
    }

    @Override
    public boolean onUnbind(Intent intent) {
        return super.onUnbind(intent);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(TAG, "onStartCommand");
        return START_STICKY;
    }

    @SuppressLint({"ForegroundServiceType", "WrongConstant"})
    @RequiresApi(api = Build.VERSION_CODES.O)
    @Override
    public void onCreate() {
        super.onCreate();
        Log.d(TAG, "onCreate");

        NotificationChannel channel = new NotificationChannel(NOTIFICATION_ID, NOTIFICATION_NAME,
                NotificationManager.IMPORTANCE_LOW);
        NotificationManager manager = (NotificationManager) this
                .getSystemService(Context.NOTIFICATION_SERVICE);
        manager.createNotificationChannel(channel);
        Notification notification = new Notification.Builder(this, NOTIFICATION_ID).build();
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
            startForeground(FOREGROUND_SERVICE_ID, notification);
        } else {
            startForeground(FOREGROUND_SERVICE_ID, notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_SPECIAL_USE);
        }
    }

    @Override
    public void onDestroy() {
        Log.d(TAG, "onDestroy");
        super.onDestroy();
    }
}