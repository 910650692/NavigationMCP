package com.fy.navi.hmi.hud;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * HUD Model
 */
public class HUDViewModel extends BaseHUDViewModel {

    public HUDViewModel(@NonNull Application application) {
        super(application);
    }
}

