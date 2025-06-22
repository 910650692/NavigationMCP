package com.sgm.navi.service.adapter.position.bls.manager;

import android.content.Context;
import android.location.Location;

import com.android.utils.log.Logger;
import com.autonavi.gbl.pos.PosService;
import com.autonavi.gbl.pos.model.LocDataType;
import com.autonavi.gbl.pos.model.LocGnss;
import com.autonavi.gbl.pos.model.LocSignData;
import com.autonavi.gbl.pos.model.LocSpeedometer;
import com.autonavi.gbl.pos.observer.IPosSensorParaObserver;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.position.IPositionAdapterCallback;
import com.sgm.navi.service.adapter.position.PositionConstant;
import com.sgm.navi.service.adapter.position.bls.PositionBlsStrategy;
import com.sgm.navi.service.adapter.position.bls.comm.LocationUtil;
import com.sgm.navi.service.adapter.position.bls.gnss.GnssManager;
import com.sgm.navi.service.adapter.position.bls.listener.IDrSensorListener;
import com.sgm.navi.service.adapter.position.bls.listener.ILocationListener;
import com.sgm.navi.service.adapter.position.bls.sensor.DrSensorManager;
import com.sgm.navi.service.define.position.LocGpgsvWrapper;
import com.sgm.navi.service.define.position.LocMode;

import java.util.List;

public class LocSigFusionManager implements IPosSensorParaObserver, ILocationListener, IDrSensorListener {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private final PositionBlsStrategy mPositionBlsStrategy;
    private PosService mPosService;
    private boolean mIsDrMode;// 是否开启DR模式
    private DrSensorManager mSensorManager;
    private GnssManager mGnssManager;

    public LocSigFusionManager(Context context, LocMode locMode, PositionBlsStrategy positionBlsStrategy) {
        Logger.i(TAG, "locMode：" + locMode);
        this.mPositionBlsStrategy = positionBlsStrategy;
        mPosService = mPositionBlsStrategy.getPosService();
        mGnssManager = new GnssManager(context, this, locMode);
        if (locMode == LocMode.DrBack) {
            mSensorManager = new DrSensorManager(context, this);
        }
    }

    public void init() {
        Logger.i(TAG, " init");
        if (mPosService != null) {
            mPosService.addSensorParaObserver(this);
        }
        if (mSensorManager != null) {
            mSensorManager.init();
        }
        if (mGnssManager != null) {
            mGnssManager.init();
        }
    }

    public void unInit() {
        Logger.i(TAG, " unInit");
        if (mPosService != null) {
            mPosService.removeSensorParaObserver(this);
        }
        if (mSensorManager != null) {
            mSensorManager.uninit();
        }
        if (mGnssManager != null) {
            mGnssManager.unInit();
        }
    }

    @Override
    public void onLocGyroInfo(LocSignData locSignData) {
        if (null == locSignData) {
            return;
        }
        locSignData.dataType = LocDataType.LocDataGyro;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    @Override
    public void onLocAcce3dInfo(LocSignData locSignData) {
        if (null == locSignData) {
            return;
        }
        locSignData.dataType = LocDataType.LocDataAcce3D;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    @Override
    public void onLocPulseInfo(LocSignData locSignData) {
        if (null == locSignData) {
            return;
        }
        locSignData.dataType = LocDataType.LocDataPulse;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    @Override
    public void onGpsInfo(LocGnss locGnss) {
        if (null == locGnss) {
            return;
        }
        LocSignData data = new LocSignData();
        data.dataType = LocDataType.LocDataGnss;
        data.gnss = locGnss;
        mPositionBlsStrategy.setSignInfo(data);
        mPositionBlsStrategy.updateSdkLocStatus(true);
    }

    @Override
    public void onGSVInfo(LocGpgsvWrapper wrapper) {
        if (wrapper == null) {
            return;
        }
        LocSignData locSignData = new LocSignData();
        locSignData.dataType = LocDataType.LocDataGpgsv;
        locSignData.gpgsv = wrapper.locGpgsv;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    @Override
    public void onLocMeterInfo(LocSpeedometer locSpeedometer) {
        if (locSpeedometer == null) {
            return;
        }
        LocSignData locSignData = new LocSignData();
        locSignData.speedometer = locSpeedometer;
        locSignData.dataType = LocDataType.LocDataSpeedometer;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    @Override
    public void onSatelliteNum(int num) {
        if (mPositionBlsStrategy != null && mPositionBlsStrategy.getCallBack() != null) {
            List<IPositionAdapterCallback> callbacks = mPositionBlsStrategy.getCallBack();
            if (callbacks != null) {
                for (IPositionAdapterCallback callback : callbacks) {
                    callback.onSatelliteNum(num);
                }
            }
        }
    }

    @Override
    public void onGpsCheckTimeOut() {
        if (mPositionBlsStrategy != null) {
            mPositionBlsStrategy.updateGnssState(false);
        }
    }

    public void setCustomPOI(double lon, double lat) {
        Location location = new Location(android.location.LocationManager.GPS_PROVIDER);
        if (lon != 0 && lat != 0) {
            location.setLongitude(lon);
            location.setLatitude(lat);
            onGpsInfo(LocationUtil.getLocGnssByLocation(location, 9));
        }
        Logger.i(TAG, " setCustomPOI customPOI lon=" + lon + " lat=" + lat);
    }

    public void onMeterSpeedChanged(float speed) {
        if (mGnssManager != null) {
            mGnssManager.onMeterSpeedChanged(speed);
        }
    }

    public void onPulseSpeedChanged(float speed) {
        if (mSensorManager != null) {
            mSensorManager.onPulseSpeedChanged(speed);
        }
    }

    public void onGearChanged(int gear) {
        if (mSensorManager != null) {
            mSensorManager.onGearChanged(gear);
        }
    }

    public void setDrEnable(boolean enable) {
        mIsDrMode = enable;
    }

    @Override
    public void onSensorParaUpdate(String s) {
        Logger.d(TAG, "onSensorParaUpdate info：" + s + ",mIsDrMode：" + mIsDrMode);
        if (mIsDrMode) {
            mPositionBlsStrategy.onLocAnalysisResult(PositionConstant.DRDebugEvent.DR_TYPE_SENSOR, s);
        }
    }
}
