package com.sgm.navi.service.adapter.position.bls;

import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.position.IPositionAdapterCallback;
import com.sgm.navi.service.adapter.position.IPositionApi;
import com.sgm.navi.service.adapter.position.PositionConstant;
import com.sgm.navi.service.adapter.position.VehicleSpeedController;
import com.sgm.navi.service.adapter.position.bls.manager.LocSigFusionManager;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.position.ISpeedCallback;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.position.LocMode;
import com.sgm.navi.service.define.position.PositionConfig;

import java.math.BigInteger;

/**
 * @Description LocationManager类
 * @Author lvww
 * @date 2024/11/24
 */
public class PositionAdapterImpl implements IPositionApi, ISpeedCallback {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private PositionBlsStrategy positionStrategy;
    private LocSigFusionManager mLocSigFusionManager;
    private LocMode mLocMode = LocMode.DrBack;
    private VehicleSpeedController mVehicleSpeedController;

    public PositionAdapterImpl() {
        positionStrategy = new PositionBlsStrategy(AppCache.getInstance().getMContext());
    }

    @Override
    public void registerCallback(IPositionAdapterCallback callback) {
        positionStrategy.registerCallback(callback);
    }

    @Override
    public void unregisterCallback(IPositionAdapterCallback callback) {
        positionStrategy.unregisterCallback();
    }

    @Override
    public void unInitPositionService() {

    }

    @Override
    public boolean init() {
        mLocMode = PositionConstant.isDrBack ? LocMode.DrBack : LocMode.GNSS;
        if (mLocMode == LocMode.DrBack) {
            mVehicleSpeedController = new VehicleSpeedController(AppCache.getInstance().getMContext(), this);
            mVehicleSpeedController.registerCallback();
        }
        boolean initResult = positionStrategy.initLocEngine(mLocMode, new PositionConfig());
        Logger.i(TAG, "initLocEngine: " + initResult + ",mLocMode：" + mLocMode);
        return initResult;
    }

    @Override
    public LocInfoBean getLastCarLocation() {
        return positionStrategy.getLastCarLocation();
    }

    @Override
    public void startPosition() {
        init(mLocMode);
    }

    @Override
    public void stopPosition() {
        positionStrategy.doStopLocate();
        unInit();
    }

    @Override
    public void saveLocStorage() {
        positionStrategy.saveLocStorage();
    }

    @Override
    public void switchParallelRoad(int switchRoadType, BigInteger roadId) {
        positionStrategy.switchParallelRoad(switchRoadType, roadId);
    }

    @Override
    public GeoPoint wgs84ToGcj02(GeoPoint geoPoint) {
        return positionStrategy.wgs84ToGcj02(geoPoint);
    }

    /***初始化定位模块***/
    public void init(LocMode locMode) {
        mLocSigFusionManager = new LocSigFusionManager(AppCache.getInstance().getMApplication().getApplicationContext(),
                locMode, positionStrategy);
        mLocSigFusionManager.init();
    }

    public void unInit() {
        if (mLocSigFusionManager != null) {
            mLocSigFusionManager.unInit();
        }
        if (positionStrategy != null) {
            positionStrategy.uninitLocEngine();
        }
        if (mVehicleSpeedController != null) {
            mVehicleSpeedController.unregisterCallback();
        }
    }

    @Override
    public void onSpeedChanged(float speed) {
        Logger.i(TAG, "Current speed: " + speed);
        if (mLocSigFusionManager != null) {
            mLocSigFusionManager.onMeterSpeedChanged(speed);
        }
    }

    @Override
    public void onPulseSpeedChanged(float speed) {
        if (mLocSigFusionManager != null) {
            mLocSigFusionManager.onPulseSpeedChanged(speed);
        }
    }

    @Override
    public void onGearChanged(int gear) {
        if (mLocSigFusionManager != null) {
            mLocSigFusionManager.onGearChanged(gear);
        }
        if (positionStrategy != null) {
            positionStrategy.onGearChanged(gear);
        }
    }

    @Override
    public void setDrEnable(boolean enable) {
        if (mLocSigFusionManager != null) {
            mLocSigFusionManager.setDrEnable(enable);
        }
    }

    @Override
    public void setCustomPOI(double lon, double lat) {
        if (mLocSigFusionManager != null) {
            mLocSigFusionManager.setCustomPOI(lon, lat);
        }
    }

    @Override
    public void locationLogSwitch(boolean isOpen) {
        if (positionStrategy != null) {
            positionStrategy.locationLogSwitch(isOpen);
        }
    }

}
