package com.fy.navi.hmi.hud;

import android.app.ActivityOptions;
import android.content.Context;
import android.content.Intent;
import android.hardware.display.DisplayManager;
import android.os.Bundle;
import android.util.Log;
import android.view.Display;

import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;


import com.android.utils.log.Logger;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.search.PoiInfoEntity;

public class HUDManager {
    private static final String TAG = "HUDManager";

    private HUDManager() {}

    private static final class Holder {
        private static final HUDManager instance = new HUDManager();
    }

    public static HUDManager getInstance() {
        return Holder.instance;
    }
    public void startInitService() {
        Logger.i(TAG, "startInitEngine");
        Intent intent = new Intent(AppContext.getInstance().getMContext(), NaviService.class);
        ActivityCompat.startForegroundService(AppContext.getInstance().getMContext(), intent);
    }
    public void startHudActivity() {
        Logger.i(TAG, "startHudActivity");
        Context context = AppContext.getInstance().getMContext();
        DisplayManager displayManager = (DisplayManager) context.getSystemService(Context.DISPLAY_SERVICE);
        int secondDisplayId = 0;
        if (displayManager != null) {
            for (Display display : displayManager.getDisplays()) {
                Log.d(TAG, "Display: name=" + display.getName() + ", id=" + display.getDisplayId());
                if (display.getDisplayId() != 0) {
                    secondDisplayId = display.getDisplayId();
                }
            }
        }
        boolean isFeatureSupported = context.getPackageManager().hasSystemFeature(android.content.pm.PackageManager.FEATURE_ACTIVITIES_ON_SECONDARY_DISPLAYS);
        Log.d(TAG, "Supports activities on secondary displays: " + isFeatureSupported);
        if (isFeatureSupported && secondDisplayId > 0) {
            ActivityOptions options = ActivityOptions.makeBasic();
            options.setLaunchDisplayId(secondDisplayId);
            Intent intent = new Intent(context, HUDMapActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            context.startActivity(intent, options.toBundle());
        } else {
            Log.e(TAG, "Secondary display not supported or not found.");
        }
    }
    public void startHudActivityOnPrimaryDisplay() {
        Log.d(TAG, "Starting HUDMapActivity on primary display");
        Context context = AppContext.getInstance().getMContext();
        Intent intent = new Intent(context, HUDMapActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(intent);
    }
}
