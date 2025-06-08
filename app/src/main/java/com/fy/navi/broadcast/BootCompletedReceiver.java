package com.fy.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import androidx.core.app.ActivityCompat;

import com.fy.navi.NaviService;
import com.fy.navi.mapservice.util.Logger;
import com.fy.navi.service.AppCache;

/**
 * 开机广播
 */
public class BootCompletedReceiver extends BroadcastReceiver {
    private static final String TAG = BootCompletedReceiver.class.getSimpleName();

    @Override
    public void onReceive(Context context, Intent intent) {
        if (Intent.ACTION_BOOT_COMPLETED.equals(intent.getAction())) {
            // 执行开机后的逻辑
            Logger.i(TAG, "Device booted! 启动NaviService");
            Intent naviServiceIntent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
            ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), naviServiceIntent);
        }
    }

}
