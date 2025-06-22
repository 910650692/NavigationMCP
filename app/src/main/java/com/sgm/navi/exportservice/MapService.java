package com.sgm.navi.exportservice;

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

import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;

import com.android.utils.log.Logger;

public class MapService extends Service {
    private static final String TAG = MapService.class.getSimpleName();

    private static final String NOTIFICATION_ID = "101";
    private static final String NOTIFICATION_NAME = "MapService";
    private static final int FOREGROUND_SERVICE_ID = 1;

    @Nullable
    @Override
    public IBinder onBind(final Intent intent) {
        final String packageName = intent.getStringExtra("packageName");
        Logger.d(TAG, "onBind: " + packageName);
        return BinderPool.getInstance();
    }

    @Override
    public boolean onUnbind(final Intent intent) {
        return super.onUnbind(intent);
    }

    @Override
    public int onStartCommand(final Intent intent, final int flags, final int startId) {
        Logger.d(TAG, "onStartCommand");
        return START_STICKY;
    }

    @SuppressLint({"ForegroundServiceType", "WrongConstant"})
    @RequiresApi(api = Build.VERSION_CODES.O)
    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");

        final NotificationChannel channel = new NotificationChannel(NOTIFICATION_ID, NOTIFICATION_NAME,
                NotificationManager.IMPORTANCE_LOW);
        final NotificationManager manager = (NotificationManager) this
                .getSystemService(Context.NOTIFICATION_SERVICE);
        manager.createNotificationChannel(channel);
        final Notification notification = new Notification.Builder(this, NOTIFICATION_ID).build();
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
            startForeground(FOREGROUND_SERVICE_ID, notification);
        } else {
            startForeground(FOREGROUND_SERVICE_ID, notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_SPECIAL_USE);
        }
    }

    @Override
    public void onDestroy() {
        Logger.d(TAG, "onDestroy");
        super.onDestroy();
    }
}