package com.sgm.navi.hmi.launcher;

import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.database.ContentObserver;
import android.net.Uri;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.RemoteException;
import android.provider.Settings;
import android.util.Log;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.AppCache;
import com.patac.launcher.ILauncherCallback;
import com.patac.launcher.ILauncherModeManager;
import com.patac.launcher.PatacLauncherModeConfig;

import java.util.concurrent.ScheduledFuture;

public class FloatViewManager {
    private static final String TAG = "FloatViewManager";
    private boolean isServiceConnect = false;
    private ILauncherModeManager mLauncherModeManager;
    /* 回调监听左上角 导航小卡窗口是否显示*/
    private static ILauncherCallback mLauncherCallback = new ILauncherCallback.Stub() {
        @Override
        public void onNavigationCardVisible(boolean b) throws RemoteException {
            // b - true 显示； false 隐藏
            LauncherWindowService.getInstance().showOrHideFloatView(b);
            Logger.i(TAG, "onNavigationCardVisible b = " + b);
        }

        @Override
        public void onLauncherStateChanged(int i) throws RemoteException {
            // Nothing to do , reserved.
            Logger.i(TAG, "onLauncherStateChanged i = " + i);
        }
    };
    private IBinder.DeathRecipient mDeathRecipient = () -> {
        Log.i(TAG, "binderDied()");
        isServiceConnect = false;
        bindLauncherService();
    };
    private final ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName componentName, IBinder service) {
            mLauncherModeManager = ILauncherModeManager.Stub.asInterface(service);
            isServiceConnect = mLauncherModeManager != null;
            Log.i(TAG, "onServiceConnected()");
            if (mLauncherModeManager != null) {
                try {
                    mLauncherModeManager.registerLauncherCallback(mLauncherCallback);
                } catch (RemoteException e) {
                    Logger.e(TAG, "registerLauncherCallback failed", e.getMessage());
                }
            }
            try {
                service.linkToDeath(mDeathRecipient, 0);
            } catch (RemoteException e) {
                Logger.e(TAG, "service.linkToDeath failed", e.getMessage());
            }

        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {
            Logger.i(TAG, "onServiceDisconnected()");
            isServiceConnect = false;
        }
    };

    private ScheduledFuture scheduledFuture;
    private long DELAY_TIME = 15;
    private static final String DESKTOP_MODE_KEY = "desktop_mode";
    private int currentDeskMode;
    private final Uri uri = Settings.Global.getUriFor(DESKTOP_MODE_KEY);
    private final Handler handler = new Handler(Looper.getMainLooper());
    private ContentResolver mContentResolver;
    private ContentObserver observer = new ContentObserver(handler) {
        @Override
        public void onChange(boolean selfChange, @Nullable Uri uri) {
            super.onChange(selfChange, uri);
            currentDeskMode = Settings.Global.getInt(mContentResolver, DESKTOP_MODE_KEY, DesktopMode.KANZI_MODE.getValue());
            Logger.i(TAG, "onChange", selfChange, currentDeskMode);
        }
    };


    /***
     * 主动获取Launcher桌面方法：
     * @return
     */
    public int getDesktopMode() {
        currentDeskMode = Settings.Global.getInt(mContentResolver, DESKTOP_MODE_KEY, DesktopMode.KANZI_MODE.getValue());
        Logger.i(TAG, "getDesktopMode", currentDeskMode);
        return currentDeskMode;
    }

    private FloatViewManager() {
        mContentResolver = AppCache.getInstance().getMApplication().getContentResolver();
        mContentResolver.registerContentObserver(uri, true, observer);
        currentDeskMode = getDesktopMode();
    }

    private static final class Holder {
        private static final FloatViewManager instance = new FloatViewManager();
    }

    public static FloatViewManager getInstance() {
        return Holder.instance;
    }

    public void bindLauncherService() {
        if (isServiceConnect) {
            Logger.i(TAG, "bindLauncherService", "service had connected!");
            return;
        }
        Intent intent = new Intent(PatacLauncherModeConfig.ACTION);
        intent.setPackage("com.patac.launcher");
        isServiceConnect = AppCache.getInstance().getMContext().bindService(intent, mConnection, Context.BIND_AUTO_CREATE);
        Logger.i(TAG, "bindLauncherService", "bindResult:" + isServiceConnect);
    }

    public void unBindLauncherService() {
        if (isServiceConnect) {
            Intent intent = new Intent(PatacLauncherModeConfig.ACTION);
            intent.setPackage("com.patac.launcher");
            AppCache.getInstance().getMContext().unbindService(mConnection);
        }
    }

    /***
     * UE-[https://peedp.saic-gm.com/ccm/web/projects/VCS_Info4.0_High_Platform_PATAC_RTC#action=com.ibm.team.workitem.viewWorkItem&id=886600]
     * 显示所有的Widgets
     */
    public void showAllCardWidgets() {
        try {
            if (isServiceConnect && !ConvertUtils.isNull(mLauncherModeManager)) {
                mLauncherModeManager.setLauncherMode(PatacLauncherModeConfig.LAUNCHER_MODE, PatacLauncherModeConfig.SHOW_APP_WIDGET_AND_WEATHER);
                Logger.i(TAG, "showAllCardWidgets-Success!");
            }
        } catch (Exception e) {
            Logger.e(TAG, "showAllCardWidgets failed", e.getMessage());
        }
    }

    public void hideAllCardWidgets() {
        stopTimer();
        try {
            if (isServiceConnect && !ConvertUtils.isNull(mLauncherModeManager)) {
                mLauncherModeManager.setLauncherMode(PatacLauncherModeConfig.LAUNCHER_MODE, PatacLauncherModeConfig.HIDE_APP_WIDGET_AND_WEATHER);
                starTimer();
                Logger.i(TAG, "hideAllCardWidgets-Success!");
            }
        } catch (Exception e) {
            Logger.e(TAG, "showAllCardWidgets failed", e.getMessage());
        }
    }

    private void starTimer() {
        Logger.i(TAG, "starTimer");
        try {
            scheduledFuture = ThreadManager.getInstance().asyncDelayWithResult(() -> {
                showAllCardWidgets();
            }, DELAY_TIME);
        } catch (Exception e) {
            Logger.i(TAG, "starTimer failed", e.getMessage());
        }
    }

    private void stopTimer() {
        Logger.i(TAG, "stopTimer start");
        try {
            if (!ConvertUtils.isNull(scheduledFuture) && !scheduledFuture.isDone()) {
                boolean result = scheduledFuture.cancel(true);
                Logger.i(TAG, "stopTimer-result", result);
            } else {
                Logger.i(TAG, "starTimer failed, scheduledFuture is null or finished!");
            }
        } catch (Exception e) {
            Logger.i(TAG, "starTimer failed", e.getMessage());
        }
    }

    public enum DesktopMode {
        WALLPAPER_MODE(0), // 壁纸桌面
        NAVIGATION_MODE(1), // 导航桌面
        KANZI_MODE(2); // 车模桌面
        private final int value;

        DesktopMode(int i) {
            this.value = i;
        }

        public int getValue() {
            return value;
        }
    }
}
