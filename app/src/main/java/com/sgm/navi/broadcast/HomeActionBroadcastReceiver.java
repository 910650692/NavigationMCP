package com.sgm.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.screen.ScreenTypeUtils;
import com.sgm.navi.service.logicpaket.map.MapPackage;
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

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent == null || intent.getAction() == null) return;
        Logger.d(TAG, "HOME_CLICK_ACTION", intent.getAction());
        if (intent.getAction().equals(HOME_CLICK_ACTION)) {
            final boolean isNaviDeskBg = FloatViewManager.getInstance().isNaviDeskBg();
            final boolean isEmpty = StackManager.getInstance().getFragmentSize(MapType.MAIN_SCREEN_MAIN_MAP.name()) <= 0;
            final boolean isFullScreen = ScreenTypeUtils.getInstance().isFullScreen();
            Logger.d(TAG, "HOME_CLICK_ACTION", isNaviDeskBg, isEmpty, isFullScreen);
            if (isNaviDeskBg && isEmpty && isFullScreen) {
                FloatViewManager.getInstance().setCardWidgetStatus(false);
                FloatViewManager.getInstance().showAllCardWidgets();
            }
            if (isEmpty && !StartService.getInstance().checkSdkIsNeedInit()) {
                final BaseActivity baseActivity = StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name());
                if (baseActivity instanceof MapActivity) {
                    ((MapActivity)baseActivity).updateUiOnHomeKeyClick();
                }
            }
        }
    }

    public static void registerHomeActionReceiver() {
        try {
            final IntentFilter intentFilter = new IntentFilter();
            intentFilter.addAction(HOME_CLICK_ACTION);
            AppCache.getInstance().getMContext().registerReceiver(new HomeActionBroadcastReceiver(), intentFilter, Context.RECEIVER_EXPORTED);
            isRegister = true;
            Logger.d(TAG, "registerHomeActionReceiver-success!");
        } catch (Exception e) {
            isRegister = false;
            Logger.e(TAG, "registerHomeActionReceiver-failed", e.getMessage());
        }
    }
}
