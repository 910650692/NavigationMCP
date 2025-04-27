package com.fy.navi.hmi.launcher;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description
 * @Author yaWei
 * @date 2025/2/18
 */
public class BaseLauncherDeskViewModel extends BaseViewModel<MapLauncherDeskActivity, LauncherDeskModel> {
    public ObservableField<Boolean> mapVisibility;
    public ObservableField<Boolean> privacyVisibility;

    public BaseLauncherDeskViewModel(@NonNull Application application) {
        super(application);
        mapVisibility = new ObservableField<>(false);
        privacyVisibility = new ObservableField<>(false);
    }

    @Override
    protected LauncherDeskModel initModel() {
        return new LauncherDeskModel();
    }

    public void loadMapView() {
        mModel.loadMapView();
    }

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }
}