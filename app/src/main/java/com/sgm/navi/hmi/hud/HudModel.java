package com.sgm.navi.hmi.hud;
import android.content.Intent;

import androidx.core.app.ActivityCompat;

import com.android.utils.log.Logger;

import com.android.utils.thread.ThreadManager;
import com.sgm.navi.NaviService;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.logicpaket.hud.HudPackage;
import com.sgm.navi.ui.base.BaseModel;

public class HudModel extends BaseModel<BaseHudViewModel> implements StartService.ISdkInitCallback {
    private static final String TAG = "HudModel";
    public HudModel() {
        StartService.getInstance().registerSdkCallback(TAG, this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "start navi Service");
        Intent intent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
        ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), intent);
    }

    @Override
    public void onSdkInitSuccess() {
        Logger.d(TAG, "Sdk init success");
        StartService.getInstance().unregisterSdkCallback(this);
        HudPackage.getInstance().initHudService();
        HudPackage.getInstance().createHudView(mViewModel.getMapView());
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        HudPackage.getInstance().unInitHudService();
    }
}
