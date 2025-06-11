package com.fy.navi.hmi.hud;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseHudViewModel extends BaseViewModel<HudActivity, HudModel> {

    private static final String TAG = "BaseHudViewModel";

    public BaseHudViewModel(@NonNull Application application) {
        super(application);
        Logger.d(TAG, "BaseHudViewModel initialized");
    }

    @Override
    protected HudModel initModel() {
        Logger.d(TAG, "Initializing HudModel");
        return new HudModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate called");
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy called");
    }

    public void loadMapView(){
        Logger.d(TAG, "Loading map view");
        MapPackage.getInstance().loadMapView(mView.getMapView());
    }

    public IBaseScreenMapView getMapView() {
        Logger.d(TAG, "Getting map view");
        return mView.getMapView();
    }
}
