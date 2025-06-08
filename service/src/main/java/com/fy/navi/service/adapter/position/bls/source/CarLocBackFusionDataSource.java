package com.fy.navi.service.adapter.position.bls.source;

import android.content.Context;
import android.location.LocationManager;

import com.android.utils.log.Logger;
import com.autonavi.gbl.pos.model.LocGnss;
import com.autonavi.gbl.pos.model.LocSignData;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.IPositionAdapterCallback;
import com.fy.navi.service.adapter.position.bls.PositionBlsStrategy;
import com.fy.navi.service.adapter.position.bls.gnss.GnssManager;
import com.fy.navi.service.adapter.position.bls.listener.IDrSensorListener;
import com.fy.navi.service.adapter.position.bls.listener.ILocationListener;
import com.fy.navi.service.adapter.position.bls.sensor.DrSensorManager;
import com.fy.navi.service.define.position.LocGpgsvWrapper;
import com.fy.navi.service.define.position.LocMode;

import java.util.List;

/***处理后端融合数据***/
public class CarLocBackFusionDataSource extends BaseLocBackFusionDataSource implements ILocationListener, IDrSensorListener {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private DrSensorManager mSensorManager;
    private GnssManager mGnssManager;
    private final PositionBlsStrategy mPositionBlsStrategy;

    public CarLocBackFusionDataSource(Context context, LocMode locMode, ILocBackFusionDataObserver observer,
                                      PositionBlsStrategy mPositionBlsStrategy) {
        super(context, observer);
        mGnssManager = new GnssManager(context, (LocationManager) context.getSystemService(Context.LOCATION_SERVICE), this, locMode);
        this.mPositionBlsStrategy = mPositionBlsStrategy;
        if (locMode == LocMode.DrBack) {
            mSensorManager = new DrSensorManager(context, this);
        }
    }

    @Override
    public void onGSVInfo(LocGpgsvWrapper wrapper) {
        mFusionDataObserver.onGSVInfo(wrapper);
    }

    @Override
    public void onLocationChanged(LocGnss locGnss) {
        if (locGnss == null || (locGnss.point.lat == 0 && locGnss.point.lon == 0)) {
            Logger.e(TAG, "onLocationChanged: location=null locGnss=" + locGnss);
            return;
        }
        mFusionDataObserver.onGpsInfo(locGnss);
    }

    @Override
    protected void onStart() {
        Logger.i(TAG, " onStart=");
        if (mSensorManager != null) {
            mSensorManager.setEnable(true);
        }
    }

    @Override
    protected void onStop() {
        Logger.i(TAG, " onStop=");
        if (mSensorManager != null) {
            mSensorManager.setEnable(false);
        }
    }

    @Override
    public void init() {
        Logger.i(TAG, " init");
        if (mSensorManager != null) {
            mSensorManager.init();
        }
        if (mGnssManager != null) {
            mGnssManager.init();
        }
    }

    @Override
    public void unInit() {
        Logger.i(TAG, " unInit");
        if (mSensorManager != null) {
            mSensorManager.uninit();
        }
        if (mGnssManager != null) {
            mGnssManager.unInit();
        }
    }

    @Override
    public void onLocGyroInfo(LocSignData locGyro, boolean isRaw) {
        mFusionDataObserver.onLocGyroInfo(locGyro);
    }

    @Override
    public void onLocAcce3dInfo(LocSignData locAcce3d, boolean isRaw) {
        mFusionDataObserver.onLocAcce3dInfo(locAcce3d);
    }

    @Override
    public void onLocPulseInfo(LocSignData locPulse, boolean isRaw) {
        mFusionDataObserver.onLocPulseInfo(locPulse);
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
            mPositionBlsStrategy.onGpsCheckTimeOut();
        }
    }

    @Override
    public void setRecordEnable(boolean enable) {
        super.setRecordEnable(enable);
        if (mSensorManager != null) {
            mSensorManager.setRecordRaw(enable);
        }
    }

    @Override
    public void updateSensorPara(String s) {
    }

    public void onSpeedChanged(float speed) {
        if (mSensorManager != null) {
            mSensorManager.onSpeedChanged(speed);
        }
    }

    public void onGearChanged(int gear) {
        if (mSensorManager != null) {
            mSensorManager.onGearChanged(gear);
        }
    }
}
