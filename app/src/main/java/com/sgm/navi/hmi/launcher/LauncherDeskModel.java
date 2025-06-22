package com.sgm.navi.hmi.launcher;

import com.android.utils.log.Logger;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.MapTypeManager;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.ui.base.BaseModel;

/**
 * @Description
 * @Author yaWei
 * @date 2025/2/18
 */
public class LauncherDeskModel extends BaseModel<BaseLauncherDeskViewModel> implements StartService.ISdkInitCallback, IMapPackageCallback {
    private static final String TAG = "LauncherDeskModel";

    public LauncherDeskModel() {
        StartService.getInstance().registerSdkCallback(TAG, this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");
        MapPackage.getInstance().registerCallback(getMapId(), this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        MapPackage.getInstance().unRegisterCallback(getMapId(), this);
        MapPackage.getInstance().unBindMapView(mViewModel.getMapView());
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, mapTypeId, "创建成功 开始绑定视图 ");
        if (mapTypeId == getMapId()) {
            MapPackage.getInstance().bindMapView(mViewModel.getMapView());
        }
    }

    private MapType getMapId() {
        return MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId);
    }
}