package com.sgm.navi.hmi.hud;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.ui.base.BaseViewModel;

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

    public IBaseScreenMapView getMapView() {
        Logger.d(TAG, "Getting map view");
        return mView.getMapView();
    }
}
