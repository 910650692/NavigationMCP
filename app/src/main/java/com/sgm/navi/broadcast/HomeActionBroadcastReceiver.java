package com.sgm.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.RemoteException;
import android.provider.Settings;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.map.MapType;
import com.android.utils.ScreenTypeUtils;
import com.sgm.navi.ui.BuildConfig;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.base.StackManager;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/7/15
 * Description: [在这里描述文件功能]
 */
public class HomeActionBroadcastReceiver extends BroadcastReceiver {
    private static final String TAG = "HomeActionBroadcastReceiver";
    public static boolean isRegister = false;
    public static final String HOME_CLICK_ACTION = "com.patac.launcher.action.NOTIFY_LAUNCHER_RESUME";
    public static final String APPTRAY_CLICK_EVENT_ACTION = "com.patac.systemui.intent.action.ACTION_APPTRAY_CLICK_EVENT";
    private final String LAUNCHER_FOREGROUND_STATE_KEY = "launcher_foreground_state";
    private final int LAUNCHER_BACKGROUND = 0;

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent == null || intent.getAction() == null) return;
        // 区分dock栏点击Home还是导航图标
        String action = intent.getAction();
        String sourceName = intent.getStringExtra("SourceName"); // 新增参数

        // Cadi 处理卡片
        if (BuildConfig.FLAVOR.equals("cadi")) {
            Logger.d(TAG, "Cadi action = ", action);
            if (action.equals(APPTRAY_CLICK_EVENT_ACTION)){
                Logger.d(TAG, "Cadi sourceName = ", sourceName);
                if (sourceName == null){
                    handleHomeOrAppTrayClick();
                    return;
                }

                // 获取launcher是否在前台的状态 1 = 前台，0 = 后台
                int value = Settings.Global.getInt(context.getContentResolver(), LAUNCHER_FOREGROUND_STATE_KEY, LAUNCHER_BACKGROUND);
                Logger.d(TAG, "launcher 当前状态: ", value);
                if (value == 0){
                    FloatViewManager.getInstance().hideAllCardWidgets(false);
                }else {
                    try {
                        // true = 当前卡片显示
                        if (FloatViewManager.getInstance().isAppWidgetHidden()){
                            FloatViewManager.getInstance().hideAllCardWidgets(false);
                        }else {
                            handleHomeOrAppTrayClick();
                        }
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
            return;
        }

        // ND 557 处理卡片
        if (BuildConfig.FLAVOR.equals("clea_8255") || BuildConfig.FLAVOR.equals("clea_8775")) {
            Logger.d(TAG, "ND 557 action = ", action);
            if (action.equals(HOME_CLICK_ACTION)){
                Logger.d(TAG, "ND 557 sourceName = ", sourceName);
                if (sourceName == null){
                    handleHomeOrAppTrayClick();
                    return;
                }
                FloatViewManager.getInstance().hideAllCardWidgets(false);
            }
        }
    }

    private void handleHomeOrAppTrayClick() {
        final boolean isNaviDeskBg = FloatViewManager.getInstance().isNaviDeskBg();
        final boolean isEmpty = StackManager.getInstance().getFragmentSize(MapType.MAIN_SCREEN_MAIN_MAP.name()) <= 0;
        final boolean isFullScreen = ScreenTypeUtils.getInstance().isFullScreen();

        Logger.d(TAG, "HOME_CLICK_ACTION", isNaviDeskBg, isEmpty, isFullScreen);
        // 条件1：在导航桌面背景、无Fragment、全屏状态下显示卡片组件
        if (isNaviDeskBg && isEmpty && isFullScreen) {
            FloatViewManager.getInstance().setCardWidgetStatus(false);
            FloatViewManager.getInstance().showAllCardWidgets();
        }

        // 无Fragment时更新UI
        if (isEmpty) {
            final BaseActivity baseActivity = StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name());
            if (baseActivity instanceof MapActivity) {
                ((MapActivity)baseActivity).updateUiOnHomeKeyClick();
            }else {
                Logger.w(TAG, "Current activity is not MapActivity or is null");
            }
        }
    }

    public static void registerHomeActionReceiver() {
        try {
            Context context = AppCache.getInstance().getMContext();
            if (context == null) {
                isRegister = false;
                Logger.e(TAG, "registerHomeActionReceiver-failed: context is null");
                return;
            }

            final IntentFilter intentFilter = new IntentFilter();
            intentFilter.addAction(HOME_CLICK_ACTION);
            intentFilter.addAction(APPTRAY_CLICK_EVENT_ACTION);
            context.registerReceiver(new HomeActionBroadcastReceiver(), intentFilter, Context.RECEIVER_EXPORTED);
            isRegister = true;
            Logger.d(TAG, "registerHomeActionReceiver-success!");
        } catch (Exception e) {
            isRegister = false;
            Logger.e(TAG, "registerHomeActionReceiver-failed", e);
        }
    }
}
