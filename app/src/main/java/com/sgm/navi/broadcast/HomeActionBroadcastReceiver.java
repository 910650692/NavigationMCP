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
    public static final String APPTRAY_CLICK_EVENT_ACTION = "com.patac.systemui.intent.action.ACTION_APPTRAY_CLICK_EVENT";

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent == null || intent.getAction() == null) return;
        Logger.d(TAG, "HOME_CLICK_ACTION or APPTRAY_CLICK_EVENT_ACTION", intent.getAction());

        // 区分dock栏点击Home还是导航图标
        String sourceName = intent.getStringExtra("SourceName"); // 新增参数
        if (intent.getAction().equals(HOME_CLICK_ACTION)
                || (intent.getAction().equals(APPTRAY_CLICK_EVENT_ACTION) && sourceName == null)) {
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
                }else {
                    Logger.w(TAG, "Current activity is not MapActivity or is null");
                }
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
