package com.sgm.navi.broadcast;

import android.app.ActivityOptions;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;
import android.view.KeyEvent;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.map.MapType;

import java.util.Objects;

/**
 * 语言
 */
public class NaviMfcHardKeyReceiver extends BroadcastReceiver {
    private static final String TAG = NaviMfcHardKeyReceiver.class.getSimpleName();
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
        Class startCls = MapActivity.class;
        Intent intent = new Intent(AppCache.getInstance().getMContext(), startCls);
        final ActivityOptions options = ActivityOptions.makeBasic();
        options.setLaunchDisplayId(0);
        intent.putExtra(INaviConstant.PAGE_EXTRA, pageCode);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
    }
}
