package com.fy.navi.service.adapter.position.bls.manager;

import android.content.Context;
import android.location.Location;

import com.android.utils.log.Logger;
import com.autonavi.gbl.pos.PosService;
import com.autonavi.gbl.pos.model.LocDataType;
import com.autonavi.gbl.pos.model.LocGnss;
import com.autonavi.gbl.pos.model.LocSignData;
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

public class LocSigFusionManager implements ILocBackFusionDataSource.ILocBackFusionDataObserver, ILossRateAnalysisListener, IPosSensorParaObserver {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private final PositionBlsStrategy mPositionBlsStrategy;
    protected boolean mIsEnable = true;
    private final ILocBackFusionDataSource mDataSource;
    private LossRateAnalysisManager mLossRateAnalysis;
    private PosService mPosService;
    private boolean mIsDrMode;// 是否开启DR模式
    private LocMode mLocMode;

    public LocSigFusionManager(Context context, LocMode locMode, PositionBlsStrategy positionBlsStrategy) {
        Logger.i(TAG, "locMode：" + locMode);
        this.mPositionBlsStrategy = positionBlsStrategy;
        mLocMode = locMode;
        DRLogService drLogService = new DRLogService();
        mLossRateAnalysis = new LossRateAnalysisManager(this, drLogService);
        mDataSource = new CarLocBackFusionDataSource(context, locMode, this, mLossRateAnalysis);
        mPosService = mPositionBlsStrategy.getPosService();
        addObserver();
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
        mLossRateAnalysis.analysis(AnalysisType.GNSS);
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

        mLossRateAnalysis.analysisGSV(wrapper.attributes);
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
        if (mDataSource != null) {
            mDataSource.onSpeedChanged(speed);
        }
    }

    public void onGearChanged(int gear) {
        if (mDataSource != null) {
            mDataSource.onGearChanged(gear);
        }
    }

    public void setDrEnable(boolean enable) {
        mIsDrMode = enable;
        mLossRateAnalysis.setDrMode(mIsDrMode);
    }

    @Override
    public void onLossRateAnalysisResult(AnalysisType type, String allResult) {
        Logger.d(TAG, "onLossRateAnalysisResult AnalysisType：" + type + ",info：" + allResult + ",mIsDrMode：" + mIsDrMode);
        if (mIsDrMode) {
            mPositionBlsStrategy.onLocAnalysisResult(PositionConstant.DRDebugEvent.DR_LOSS_RATE, allResult);
        }
    }

    @Override
    public void onSensorParaUpdate(String s) {
        Logger.d(TAG, "onSensorParaUpdate info：" + s + ",mIsDrMode：" + mIsDrMode);
        if (mIsDrMode) {
            mPositionBlsStrategy.onLocAnalysisResult(PositionConstant.DRDebugEvent.DR_TYPE_SENSOR, s);
        }
        mDataSource.updateSensorPara(s);
    }
}
