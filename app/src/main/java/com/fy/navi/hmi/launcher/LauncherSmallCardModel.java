package com.fy.navi.hmi.launcher;

import android.content.Intent;

import androidx.core.app.ActivityCompat;

import com.android.utils.log.Logger;
import com.fy.navi.NaviService;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.StartService;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.ui.base.BaseModel;

public class LauncherSmallCardModel extends BaseModel<BaseLauncherSmallCardViewModel> implements StartService.ISdkInitCallback, IMapPackageCallback {
    private static final String TAG = "LauncherSmallCardModel";
    private boolean mapLoadStatus = false;

    public LauncherSmallCardModel() {
        StartService.getInstance().registerSdkCallback(this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Intent intent = new Intent(AppContext.getInstance().getMContext(), NaviService.class);
        ActivityCompat.startForegroundService(AppContext.getInstance().getMContext(), intent);
    }

    @Override
    public void onSdkInitSuccess() {
        if (mapLoadStatus) return;
        Logger.d(TAG, "引擎初始化成功");
        MapPackage.getInstance().initMapView(mViewModel.getMapView());
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.d(TAG, "引擎初始化失败");
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        Logger.d(TAG, "底图渲染成功");
        mapLoadStatus = true;
    }
}