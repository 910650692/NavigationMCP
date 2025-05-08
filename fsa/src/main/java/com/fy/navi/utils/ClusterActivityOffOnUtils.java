package com.fy.navi.utils;

import android.app.ActivityOptions;
import android.content.Intent;
import android.hardware.display.DisplayManager;
import android.os.Build;
import android.view.Display;

import com.android.utils.log.Logger;
import com.fy.navi.fsa.FsaConstant;
import com.fy.navi.service.AppContext;

public class ClusterActivityOffOnUtils {
    public static void offOnClusterActivity(final boolean isOpen) {
        int secondeDid = 2; // 仪表的DisplayId
        if (!"gm".equals(Build.MANUFACTURER)) { // 如果是非车机环境
            final DisplayManager displayManager = AppContext.getInstance().getMContext().getSystemService(DisplayManager.class);
            for (Display display : displayManager.getDisplays()) {
                Logger.d(FsaConstant.FSA_TAG, "dispaly: " + display.getName() + ", id " + display.getDisplayId() + " :" + display);
                if (display.getDisplayId() != 0) {
                    secondeDid = display.getDisplayId();
                    break;
                }
            }
        }
        if (isOpen) {
            final ActivityOptions options = ActivityOptions.makeBasic();
            options.setLaunchDisplayId(secondeDid);
            final Intent intent = new Intent();
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setAction("com.fy.navi.hmi.cluster.ClusterActivity");
            intent.putExtra("isOpen", isOpen);
            AppContext.getInstance().getMContext().startActivity(intent, options.toBundle());
        } else {
            Logger.d(FsaConstant.FSA_TAG, "close ClusterActivity");
            Intent closeIntent = new Intent("com.fy.navi.hmi.cluster.ClusterActivity");
            AppContext.getInstance().getMContext().sendBroadcast(closeIntent);
        }
    }
}
