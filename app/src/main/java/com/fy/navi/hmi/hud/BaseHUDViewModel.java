package com.fy.navi.hmi.hud;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * HUD Model
 */
public class BaseHUDViewModel extends BaseViewModel<HUDMapActivity, HUDModel> {

    public BaseHUDViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected HUDModel initModel() {
        return new HUDModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }
}

