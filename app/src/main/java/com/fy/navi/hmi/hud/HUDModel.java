package com.fy.navi.hmi.hud;

import android.app.ActivityOptions;
import android.content.Context;
import android.content.Intent;
import android.hardware.display.DisplayManager;
import android.view.Display;

import androidx.core.app.ActivityCompat;

import com.android.utils.log.Logger;
import com.fy.navi.NaviService;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.StartService;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.ui.base.BaseModel;


/**
 * HUD Model
 */
public class HUDModel extends BaseModel<BaseHUDViewModel> implements IMapPackageCallback, StartService.ISdkInitCallback {
    private static final String TAG = "HUDModel";
    private final MapPackage mapPackage;
    private boolean mapLoadStatus = false;

    public HUDModel() {
        mapPackage = MapPackage.getInstance();
        StartService.getInstance().registerSdkCallback(this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");
        Logger.i(TAG, "startInitEngine");
        Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, "start navi Service");
        Intent intent = new Intent(AppContext.getInstance().getMContext(), NaviService.class);
        ActivityCompat.startForegroundService(AppContext.getInstance().getMContext(), intent);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        mapPackage.unRegisterCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mapPackage.unBindMapView(mViewModel.getMapView());
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        mapLoadStatus = true;
    }

    @Override
    public void onSdkInitSuccess() {
        if(mapLoadStatus) return;
        mapPackage.initMapView(mViewModel.getMapView());
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.i(TAG, "Sdk init fail");
        // TODO: 2025/5/5 什么也不用做
    }

    public void startHudActivity() {
        Logger.i(TAG, "startHudActivity");
        Context context = AppContext.getInstance().getMContext();
        DisplayManager displayManager = (DisplayManager) context.getSystemService(Context.DISPLAY_SERVICE);
        int secondDisplayId = 0;
        if (displayManager != null) {
            for (Display display : displayManager.getDisplays()) {
                Logger.d(TAG, "Display: name=" + display.getName() + ", id=" + display.getDisplayId());
                if (display.getDisplayId() != 0) {
                    secondDisplayId = display.getDisplayId();
                }
            }
        }
        boolean isFeatureSupported = context.getPackageManager().hasSystemFeature(android.content.pm.PackageManager.FEATURE_ACTIVITIES_ON_SECONDARY_DISPLAYS);
        Logger.d(TAG, "Supports activities on secondary displays: " + isFeatureSupported);
        if (isFeatureSupported && secondDisplayId > 0) {
            ActivityOptions options = ActivityOptions.makeBasic();
            options.setLaunchDisplayId(secondDisplayId);
            Intent intent = new Intent(context, HUDMapActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            context.startActivity(intent, options.toBundle());
        } else {
            Logger.e(TAG, "Secondary display not supported or not found.");
        }
    }

    public void startHudActivityOnPrimaryDisplay() {
        Logger.d(TAG, "Starting HUDMapActivity on primary display");
        Context context = AppContext.getInstance().getMContext();
        Intent intent = new Intent(context, HUDMapActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(intent);
    }
}