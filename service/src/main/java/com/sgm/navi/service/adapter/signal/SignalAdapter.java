package com.sgm.navi.service.adapter.signal;

import android.content.Context;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.adapter.calibration.CalibrationAdapter;
import com.sgm.navi.service.define.signal.RoadConditionGroup;
import com.sgm.navi.service.define.signal.SdNavigationStatusGroup;

import java.util.Objects;

/**
 * 车辆信号
 */
public final class SignalAdapter {
    private static final String TAG = SignalAdapter.class.getSimpleName();

    private static final String SIGNAL_API_PKG = Objects.requireNonNull(SignalAdapter.class.getPackage()).getName();
    private static final String SIGNAL_API_CLS = "SignalAdapterImpl";
    private SignalApi mSignalApi;

    private static final class SInstanceHolder {
        static final SignalAdapter INSTANCE = new SignalAdapter();
    }

    private SignalAdapter() {
        if (!DeviceUtils.isCar(AppCache.getInstance().getMContext())) {
            return;
        }
        mSignalApi = (SignalApi) AdapterConfig.getObject(SIGNAL_API_PKG, SIGNAL_API_CLS);
    }

    /**
     * 初始化信号
     * @param context
     */
    public void initSignal(final Context context) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.initSignal(context);
    }

    /**
     * 注册回调
     * @param key
     * @param resultCallback
     */
    public void registerCallback(final String key, final SignalAdapterCallback resultCallback) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.registerCallback(key, resultCallback);
    }

    public static SignalAdapter getInstance() {
        return SignalAdapter.SInstanceHolder.INSTANCE;
    }

    public int getChargeSystemStatus() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getChargeSystemStatus();
    }

    public float getBatteryEnergyPercent() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getBatteryEnergyPercent();
    }

    public float getMaxBatteryEnergy() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getMaxBatteryEnergy();
    }

    public float getBatteryEnergy() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getBatteryEnergy();
    }

    public float getOutsideTemperature() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getOutsideTemperature();
    }

    public float getSpeedOfVehicle() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getSpeedOfVehicle();
    }

    public int getAcSwitchState() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getAcSwitchState();
    }

    public int getSystemState() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getSystemState();
    }

    public float getRangeRemaining() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getRangeRemaining();
    }

    public float getHighVoltageBatteryPropulsionRange() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getHighVoltageBatteryPropulsionRange();
    }

    public void setNextChargingDestination(int powerLevel, int status, int timeToArrival, int distToArrival) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setNextChargingDestination(powerLevel, status, timeToArrival, distToArrival);
    }

    public boolean getNavigationOnAdasTextToSpeachStatus() {
        if (mSignalApi == null) {
            return false;
        }
        int type = CalibrationAdapter.getInstance().adasConfigurationType();
        if (type == 8) {
            return mSignalApi.getGmcNopTtsEnabled();
        } else if (type == 9) {
            return mSignalApi.getPatacNopTtsEnabled();
        }
        return false;
    }

    public void setNaviVolume(int volume) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setNaviVolume(volume);
    }

    public int getNaviVolume() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getNaviVolume();
    }

    public void setSdNavigationStatus(SdNavigationStatusGroup sdNavigationStatusGroup) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setSdNavigationStatus(sdNavigationStatusGroup);
    }

    public void setNavigationOnAdasButtonSettingRequest(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setNavigationOnAdasButtonSettingRequest(value);
    }

    public void setNavigationOnAdasInfoNavigationStatus(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setNavigationOnAdasInfoNavigationStatus(value);
    }

    public void setDistanceToTrafficJamRoad(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setDistanceToTrafficJamRoad(value);
    }

    public void setDistanceToTrafficJamRoadAvailability(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setDistanceToTrafficJamRoadAvailability(value);
    }

    public void setDistanceOnTrafficJamRoad(float value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setDistanceOnTrafficJamRoad(value);
    }

    public void setDistanceOnTrafficJamRoadAvailability(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setDistanceOnTrafficJamRoadAvailability(value);
    }

    public void setTrafficJamRoadAverageSpeed(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setTrafficJamRoadAverageSpeed(value);
    }

    public void setTrafficJamRoadAverageSpeedAvailability(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setTrafficJamRoadAverageSpeedAvailability(value);
    }

    public void setRoadConditionGroup(RoadConditionGroup roadConditionGroup) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setRoadConditionGroup(roadConditionGroup);
    }

    public void setTotalDistanceFromStartToDestinationOnNavigation(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setTotalDistanceFromStartToDestinationOnNavigation(value);
    }

    public void setTotalPredictedTimeFromStartToDestinationOnNavigation(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setTotalPredictedTimeFromStartToDestinationOnNavigation(value);
    }

    public void setRemainDistanceToChargingStation(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setRemainDistanceToChargingStation(value);
    }

    public void setRemainTimeToChargingStationy(int value) {
        if (mSignalApi == null) {
            return;
        }

        mSignalApi.setRemainTimeToChargingStationy(value);
    }

    public void setVcuSpeedLimitArbitrationResults(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setVcuSpeedLimitArbitrationResults(value);
    }

    public void setVcuSpeedLimitArbitrationResultsAssured(int value) {
        if (mSignalApi == null) {
            return;
        }
        mSignalApi.setVcuSpeedLimitArbitrationResultsAssured(value);
    }

    public int getPredictedFuelSavingPer100km() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getPredictedFuelSavingPer100km();
    }

    public int getTotalFuelSaving() {
        if (mSignalApi == null) {
            return -1;
        }
        return mSignalApi.getTotalFuelSaving();
    }
}
