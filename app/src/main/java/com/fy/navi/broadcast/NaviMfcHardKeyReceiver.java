package com.fy.navi.broadcast;

import android.app.ActivityOptions;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Parcelable;
import android.text.TextUtils;
import android.view.KeyEvent;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.launcher.LauncherWindowService;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.hmi.startup.StartupActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.StackManager;

import java.util.Objects;

/**
 * 语言
 */
public class NaviMfcHardKeyReceiver extends BroadcastReceiver {
    private static final String TAG = NaviMfcHardKeyReceiver.class.getSimpleName();
    private final MapType MAP_TYPE = MapType.MAIN_SCREEN_MAIN_MAP;
    public static final String HARD_KEY_NAVIGATION = "gm.intent.ACTION_BROADCAST_KEY_MFC_NAVIGATION";

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent != null && Objects.equals(intent.getAction(), HARD_KEY_NAVIGATION)) {
            if (!TextUtils.equals("cadi", BuildConfig.FLAVOR)) return;
            Logger.i(TAG, "凯迪按压Navi硬按钮");
            KeyEvent event = intent.getParcelableExtra(Intent.EXTRA_KEY_EVENT);
            if (ConvertUtils.isEmpty(event)) {
                Logger.i(TAG, "event is null");
                return;
            }
            if (event.getAction() == KeyEvent.ACTION_UP) {
                Logger.i(TAG, "打开地图");
                openSelf(INaviConstant.OpenIntentPage.NONE);
            } else {
                Logger.i(TAG, "不响应");
            }
        }
    }

    public void openSelf(int pageCode) {
        Logger.i(TAG, "openSelf:" + pageCode);
        Class startCls = StartupActivity.class;
        boolean isActivityExist = StackManager.getInstance().isActivityExist(MAP_TYPE.name(), MapActivity.class);
        if (isActivityExist) {
            startCls = MapActivity.class;
        }
        Logger.i(TAG, "isActivityExist:" + isActivityExist);
        Intent intent = new Intent(AppContext.getInstance().getMContext(), startCls);
        final ActivityOptions options = ActivityOptions.makeBasic();
        options.setLaunchDisplayId(0);
        intent.putExtra(INaviConstant.PAGE_EXTRA, pageCode);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        AppContext.getInstance().getMContext().startActivity(intent, options.toBundle());
    }
}
