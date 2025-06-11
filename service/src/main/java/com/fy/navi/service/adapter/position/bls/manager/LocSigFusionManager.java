package com.fy.navi.service.adapter.position.bls.manager;

import android.content.Context;
import android.location.Location;
import android.os.SystemClock;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.pos.PosService;
import com.autonavi.gbl.pos.model.LocDataType;
import com.autonavi.gbl.pos.model.LocGnss;
import com.autonavi.gbl.pos.model.LocSignData;
import com.autonavi.gbl.pos.model.LocSpeedometer;
import com.autonavi.gbl.pos.observer.IPosSensorParaObserver;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.PositionConstant;
import com.fy.navi.service.adapter.position.bls.PositionBlsStrategy;
import com.fy.navi.service.adapter.position.bls.analysis.AnalysisType;
import com.fy.navi.service.adapter.position.bls.analysis.LossRateAnalysisManager;
import com.fy.navi.service.adapter.position.bls.comm.LocationUtil;
import com.fy.navi.service.adapter.position.bls.dr.DRLogService;
import com.fy.navi.service.adapter.position.bls.listener.ILocBackFusionDataSource;
import com.fy.navi.service.adapter.position.bls.listener.ILossRateAnalysisListener;
import com.fy.navi.service.adapter.position.bls.source.CarLocBackFusionDataSource;
import com.fy.navi.service.define.position.LocGpgsvWrapper;
import com.fy.navi.service.define.position.LocMode;

import java.math.BigInteger;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public class LocSigFusionManager implements ILocBackFusionDataSource.ILocBackFusionDataObserver , IPosSensorParaObserver {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private final PositionBlsStrategy mPositionBlsStrategy;
    protected boolean mIsEnable = true;
    private final ILocBackFusionDataSource mDataSource;
    private PosService mPosService;
    private boolean mIsDrMode;// 是否开启DR模式
    private Runnable mCustomTimer;
    private ScheduledFuture mScheduledFuture;
    private float mCarMeterSpeed = 0f;  //仪表车速 1hz 一秒一次

    public LocSigFusionManager(Context context, LocMode locMode, PositionBlsStrategy positionBlsStrategy) {
        Logger.i(TAG, "locMode：" + locMode);
        this.mPositionBlsStrategy = positionBlsStrategy;
        mDataSource = new CarLocBackFusionDataSource(context, locMode, this, mPositionBlsStrategy);
        mPosService = mPositionBlsStrategy.getPosService();
        addObserver();
        startTimerTask();
    }

    public void addObserver() {
        if (mPosService != null) {
            mPosService.addSensorParaObserver(this);
        }
    }

    public void removeObserver() {
        if (mPosService != null) {
            mPosService.removeSensorParaObserver(this);
        }
    }

    @Override
    public void onLocGyroInfo(LocSignData locSignData) {
        if (!mIsEnable || null == locSignData) {
            return;
        }
        locSignData.dataType = LocDataType.LocDataGyro;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    @Override
    public void onLocAcce3dInfo(LocSignData locSignData) {
        if (!mIsEnable || null == locSignData) {
            return;
        }
        locSignData.dataType = LocDataType.LocDataAcce3D;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    @Override
    public void onLocPulseInfo(LocSignData locSignData) {
        if (!mIsEnable || null == locSignData) {
            return;
        }
        locSignData.dataType = LocDataType.LocDataPulse;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    @Override
    public void onGpsInfo(LocGnss locGnss) {
        if (!mIsEnable || null == locGnss) {
            return;
        }
        mPositionBlsStrategy.setGnssInfo(locGnss);
    }

    @Override
    public void onGSVInfo(LocGpgsvWrapper wrapper) {
        if (!mIsEnable) {
            return;
        }
        LocSignData locSignData = new LocSignData();
        locSignData.dataType = LocDataType.LocDataGpgsv;
        locSignData.gpgsv = wrapper.locGpgsv;
        mPositionBlsStrategy.setSignInfo(locSignData);
    }

    public void init() {
        mDataSource.init();
    }

    public void unInit() {
        mDataSource.unInit();
    }

    public void setDrBackFusionEnable(boolean enable) {
        mDataSource.setDrBackFusionEnable(enable);
        mIsEnable = enable;
    }

    public void setRecordEnable(boolean enable) {
        mDataSource.setRecordEnable(enable);
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

    public void onSpeedChanged(float speed) {
        mCarMeterSpeed = speed;
    }

    public void onPulseSpeedChanged(float speed) {
        if (mDataSource != null) {
            mDataSource.onPulseSpeedChanged(speed);
        }
    }

    public void onGearChanged(int gear) {
        if (mDataSource != null) {
            mDataSource.onGearChanged(gear);
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
        mDataSource.updateSensorPara(s);
    }


    private void startTimerTask() {
        Logger.i(TAG, "LocSpeedometer");
        stopTimerTask();
        mCustomTimer = new Runnable() {
            @Override
            public void run() {
                // 创建LocSpeedometer对象
                LocSpeedometer locSpeedometer = new LocSpeedometer();
                locSpeedometer.value = mCarMeterSpeed;
                locSpeedometer.tickTime = new BigInteger(String.valueOf(SystemClock.elapsedRealtime()));
                LocSignData locSignData = new LocSignData();
                locSignData.speedometer = locSpeedometer;
                mPositionBlsStrategy.setSignInfo(locSignData);
                Logger.d("LocSpeedometer",mCarMeterSpeed);
            }
        };
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(mCustomTimer, 0, 1000, TimeUnit.MILLISECONDS);
    }

    private void stopTimerTask() {
        Logger.i(TAG, "stopTimerTask");
        if (mScheduledFuture != null) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mCustomTimer = null;
        }
    }
}
