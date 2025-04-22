package com.fy.navi.hmi.hud;

import com.android.utils.log.Logger;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.logicpaket.engine.IEngineObserver;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.ui.base.BaseModel;


/**
 * HUD Model
 */
public class HUDModel extends BaseModel<HUDViewModel>implements IMapPackageCallback, IEngineObserver {
    private static final String TAG = "HUDModel";
    private final MapPackage mapPackage;

    private boolean isMapLoadSuccess = false;
    public HUDModel() {
        mapPackage = MapPackage.getInstance();
    }
    /**
     * 创建
     */
    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");
    }
    /**
     * 销毁
     */
    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        mapPackage.unRegisterCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mapPackage.unBindMapView(mViewModel.getMapView());
    }
    @Override
    public void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
    }
    /**
     * 加载地图
     */
    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        Logger.i(TAG, "loadMapView mapSurfaceView"+mapSurfaceView);
        mapPackage.initMapView(mapSurfaceView);
    }

    @Override
    public void onInitEngineSuccess() {
        Logger.i(TAG, "onInitEngineSuccess");
        mViewModel.afterMapEnginInit(true);
    }
    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        Logger.i(TAG, "onMapLoadSuccess", "mapTypeId:" + mapTypeId.name());
        if (mapTypeId == MapType.HUD_MAP) {
            isMapLoadSuccess = true;
        }
    }

    @Override
    public void onInitEngineFail(int code, String msg) {
        Logger.i(TAG, "onInitEngineFail", "error msg:" + msg);
        mViewModel.afterMapEnginInit(false);
    }

    public void loadMapView() {
        Logger.i(TAG, "loadMapView", "isMapLoadSuccess:" + isMapLoadSuccess);
        if (!isMapLoadSuccess) {
            mapPackage.initMapView(mViewModel.getMapView());
        }
    }
}