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
import android.view.MotionEvent;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.AppCache;
import com.patac.launcher.ILauncherCallback;
import com.patac.launcher.ILauncherModeManager;
import com.patac.launcher.PatacLauncherModeConfig;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.vrbridge.MapStateManager;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledFuture;

public class FloatViewManager {
    private final int MOVE_DISTANCE = 20;
    private int startX;
    private int startY;
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
            int launcherDeskMode =MapStateManager.getInstance().getLauncherDeskMode();
            currentDeskMode = Settings.Global.getInt(mContentResolver, DESKTOP_MODE_KEY, DesktopMode.KANZI_MODE.getValue());
            Logger.i(TAG, "onChange", selfChange, currentDeskMode);
            MapStateManager.getInstance().setLauncherDeskMode(currentDeskMode);
            notifyDeskModeChanged();

            if(StartService.getInstance().checkSdkIsNeedInit()) return;
            if(launcherDeskMode == DesktopMode.NAVIGATION_MODE.value || currentDeskMode == DesktopMode.NAVIGATION_MODE.value){
                String mapStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
                if (mapStatus.equals(NaviStatus.NaviStatusType.ROUTING)) {
                    RoutePackage.getInstance().abortRequest(MapType.MAIN_SCREEN_MAIN_MAP);
                    return;
                }
                if (mapStatus.equals(NaviStatus.NaviStatusType.SELECT_ROUTE)) {
                    RoutePackage.getInstance().clearRestArea(MapType.MAIN_SCREEN_MAIN_MAP);
                    RoutePackage.getInstance().clearWeatherView(MapType.MAIN_SCREEN_MAIN_MAP);
                    RoutePackage.getInstance().clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
                    LayerPackage.getInstance().clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
                }
            }
        }
    };


    /***
     * 主动获取Launcher桌面方法：
     * @return
     */
    public void getDesktopMode() {
        ThreadManager.getInstance().execute(() -> {
            currentDeskMode = Settings.Global.getInt(mContentResolver, DESKTOP_MODE_KEY, DesktopMode.KANZI_MODE.getValue());
            Logger.i(TAG, "getDesktopMode", currentDeskMode);
            MapStateManager.getInstance().setLauncherDeskMode(currentDeskMode);
        });
    }

    private final CopyOnWriteArrayList<IDeskBackgroundChangeListener> mDeskBackgroundChangeListeners = new CopyOnWriteArrayList<>();
    private final CopyOnWriteArrayList<OnDeskCardVisibleStateChangeListener> mDeskCardVisibleChangeListeners = new CopyOnWriteArrayList<>();
    private FloatViewManager() {
        mContentResolver = AppCache.getInstance().getMApplication().getContentResolver();
        mContentResolver.registerContentObserver(uri, true, observer);
        getDesktopMode();
    }

    /**
     * 容器里面fragment数量发生改变
     */
    public void showAllCardWidgetsAfterFragmentSizeChanged() {
        final boolean isEmpty = StackManager.getInstance().getFragmentSize(MapType.MAIN_SCREEN_MAIN_MAP.name()) <=0 ;
        Logger.d(TAG, "showAllCardWidgetsAfterFragmentSizeChanged", isEmpty);
        if (!isNaviDeskBg()) return;
        if (isEmpty) {
            showAllCardWidgets();
        } else {
            hideAllCardWidgets(false);
        }
    }

    /***
     * 地图触摸,隐藏小卡片
     */
    public void hideWidgetsOnMapTouch(MotionEvent touchEvent) {
        Logger.d(TAG, "hideWidgetsOnMapTouch");
        if (!isNaviDeskBg()) return;
        switch (touchEvent.getAction()) {
            case MotionEvent.ACTION_DOWN:
                startX = (int) touchEvent.getX();
                startY = (int) touchEvent.getY();
                break;
            case MotionEvent.ACTION_MOVE:
            case MotionEvent.ACTION_UP:
                if (Math.abs(touchEvent.getX() - startX) >= MOVE_DISTANCE || Math.abs(touchEvent.getY() - startY) >= MOVE_DISTANCE) {
                    hideAllCardWidgets(StackManager.getInstance().getFragmentSize(MapType.MAIN_SCREEN_MAIN_MAP.name()) <= 0);
                }
                break;
            default:
                break;

        }
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
        ThreadManager.getInstance().execute(() -> {
            try {
                if (!isNaviDeskBg()) {
                    Logger.d(TAG, "showAllCardWidgets", "当前桌面背景不是导航桌面背景,不需要显示！");
                    return;
                }
                if (isServiceConnect && !ConvertUtils.isNull(mLauncherModeManager)) {
                    mLauncherModeManager.setLauncherMode(PatacLauncherModeConfig.LAUNCHER_MODE, PatacLauncherModeConfig.SHOW_APP_WIDGET_AND_WEATHER);
                    Logger.i(TAG, "showAllCardWidgets-Success!");
                    notifyDeskCardVisibleStateChange(judgedWidgetIsVisible());
                }
            } catch (Exception e) {
                Logger.e(TAG, "showAllCardWidgets failed", e.getMessage());
            }
        });
    }

    // TODO 这里有优化空间，待优化
    public void hideAllCardWidgets(boolean isNeedStartTimer) {
        if (!isNaviDeskBg()) {
            Logger.d(TAG, "hideAllCardWidgets", "当前桌面背景不是导航桌面背景,不需要隐藏！");
            return;
        }
        ThreadManager.getInstance().execute(() -> {
            Logger.i(TAG, "hideAllCardWidgets", isNeedStartTimer);
            stopTimer();
            try {
                if (isServiceConnect && !ConvertUtils.isNull(mLauncherModeManager)) {
                    mLauncherModeManager.setLauncherMode(PatacLauncherModeConfig.LAUNCHER_MODE, PatacLauncherModeConfig.HIDE_APP_WIDGET_AND_WEATHER);
                    notifyDeskCardVisibleStateChange(judgedWidgetIsVisible());
                    if (isNeedStartTimer) {
                        starTimer();
                    }
                    Logger.i(TAG, "hideAllCardWidgets-Success!");
                }
            } catch (Exception e) {
                Logger.e(TAG, "showAllCardWidgets failed", e.getMessage());
            }
        });
    }

    private void starTimer() {
        ThreadManager.getInstance().execute(() -> {
            Logger.i(TAG, "starTimer");
            try {
                scheduledFuture = ThreadManager.getInstance().asyncDelayWithResult(() -> {
                    showAllCardWidgets();
                }, DELAY_TIME);
            } catch (Exception e) {
                Logger.i(TAG, "starTimer failed", e.getMessage());
            }
        });
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

    /***
     * 判断当前桌面是否为导航桌面
     * @return
     */
    public static boolean isNaviDeskBg(DesktopMode desktopMode) {
        return desktopMode == DesktopMode.NAVIGATION_MODE;
    }

    /***
     * 判断当前桌面是否为导航桌面
     * @return
     */
    public boolean isNaviDeskBg() {
        return currentDeskMode == DesktopMode.NAVIGATION_MODE.getValue();
    }

    /***
     * 获取当前桌面模式是否为壁纸桌面
     * @return
     */
    public boolean isBiZhiDeskBg() {
        return currentDeskMode == DesktopMode.WALLPAPER_MODE.getValue();
    }

    public void addDeskBackgroundChangeListener(IDeskBackgroundChangeListener listener) {
        if (!ConvertUtils.isNull(listener)) {
            mDeskBackgroundChangeListeners.add(listener);
        }
    }

    public void removeDeskBackgroundChangeListener(IDeskBackgroundChangeListener listener) {
        if (!ConvertUtils.isNull(listener)) {
            mDeskBackgroundChangeListeners.remove(listener);
        }
    }

    public void addDeskCardVisibleChangeListener(OnDeskCardVisibleStateChangeListener listener) {
        if (!ConvertUtils.isNull(listener)) {
            mDeskCardVisibleChangeListeners.add(listener);
        }
    }

    public void removeDeskCardVisibleChangeListener(OnDeskCardVisibleStateChangeListener listener) {
        if (!ConvertUtils.isNull(listener)) {
            mDeskCardVisibleChangeListeners.remove(listener);
        }
    }

    private void notifyDeskModeChanged() {
        mDeskBackgroundChangeListeners.forEach(listener -> listener.onDeskBackgroundChange(DesktopMode.values()[currentDeskMode]));
    }

    private void notifyDeskCardVisibleStateChange(boolean isVisible) {
        mDeskCardVisibleChangeListeners.forEach(listener -> listener.onDeskCardVisibleStateChange(isVisible));
    }

    /***
     * 判断当前桌面的Widgets是否可见
     * @return true:可见
     */
    public boolean judgedWidgetIsVisible() {
        try {
            if (isServiceConnect && !ConvertUtils.isNull(mLauncherModeManager)) {
                int mode = mLauncherModeManager.getLauncherMode(PatacLauncherModeConfig.LAUNCHER_MODE);
                Logger.d(TAG, "judgedWidgetIsVisible-mode", mode);
                return mode == PatacLauncherModeConfig.SHOW_APP_WIDGET_AND_WEATHER || mode == PatacLauncherModeConfig.SHOW_APP_WIDGET;
            } else {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
    }
}
