package com.fy.navi.hmi.hud;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * HUD Model
 */
public class HUDViewModel extends BaseViewModel<HUDMapActivity, HUDModel> {
    public ObservableField<Boolean> mapVisibility;
    public ObservableField<Boolean> privacyVisibility;

    public HUDViewModel(@NonNull Application application) {
        super(application);
        mapVisibility = new ObservableField<>(false);
        privacyVisibility = new ObservableField<>(false);
    }

    @Override
    protected HUDModel initModel() {
        return new HUDModel();
    }
    /***
     * 地图引擎初始化后回调
     * @param isSuccess true代表初始化成功
     */
    public void afterMapEnginInit(boolean isSuccess) {
        mapVisibility.set(isSuccess);
    }

    public void onPrivacyAgreed() {
        privacyVisibility.set(false);
    }

    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        if (mModel != null) {
            mModel.loadMapView(mapSurfaceView);
        }
    }
    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }
}

