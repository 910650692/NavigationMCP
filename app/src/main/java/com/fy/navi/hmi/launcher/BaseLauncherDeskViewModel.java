package com.fy.navi.hmi.launcher;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description
 * @Author yaWei
 * @date 2025/2/18
 */
public class BaseLauncherDeskViewModel extends BaseViewModel<MapLauncherDeskActivity, LauncherDeskModel> {

    public BaseLauncherDeskViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected LauncherDeskModel initModel() {
        return new LauncherDeskModel();
    }

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }
}