package com.sgm.navi.service.adapter.calibration;

import com.android.utils.DeviceUtils;
import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.AppCache;

import java.util.Map;
import java.util.Objects;

public final class CalibrationAdapter {
    private static final String CALIBRATION_API_PKG = Objects.requireNonNull(CalibrationAdapter.class.getPackage()).getName();
    private static final String CALIBRATION_API_CLS = "CalibrationAdapterImpl";
    private CalibrationApi mCalibrationApi;

    public static CalibrationAdapter getInstance() {
        return CalibrationAdapter.SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final CalibrationAdapter INSTANCE = new CalibrationAdapter();
    }

    private CalibrationAdapter() {
        if (!DeviceUtils.isCar(AppCache.getInstance().getMContext()))  {
            return;
        }
        mCalibrationApi = (CalibrationApi) AdapterConfig.getObject(CALIBRATION_API_PKG, CALIBRATION_API_CLS);
        mCalibrationApi.init();
    }

    public void registerCallback(final String key, final CalibrationAdapterCallback resultCallback) {
        if (mCalibrationApi == null) {
            return;
        }
        mCalibrationApi.registerCallback(key, resultCallback);
    }

    public void unregisterCallback(final String key) {
        if (mCalibrationApi == null) {
            return;
        }
        mCalibrationApi.unregisterCallback(key);
    }

    /**
     * powerType
     * @return int
     */
    public int powerType() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.powerType();
    }

    /**
     * brand
     * @return int
     */
    public int brand() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.brand();
    }

    /**
     * model
     * @return int
     */
    public int model() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.model();
    }

    /**
     * enableApplicationNavigation
     * @return boolean
     */
    public boolean enableApplicationNavigation() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.enableApplicationNavigation();
    }

    /**
     * laneLevelNavigatioFuncEnable
     * @return boolean
     */
    public boolean laneLevelNavigatioFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.laneLevelNavigatioFuncEnable();
    }

    /**
     * v2xMapDisplayFuncEnable
     * @return boolean
     */
    public boolean v2xMapDisplayFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.v2xMapDisplayFuncEnable();
    }

    /**
     * speedLimitInformationSource
     * @return int
     */
    public int speedLimitInformationSource() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.speedLimitInformationSource();
    }

    /**
     * adasConfigurationInfomation
     * @return int
     */
    public int adasConfigurationInfomation() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.adasConfigurationInfomation();
    }

    /**
     * adasConfigurationType
     * @return int
     */
    public int adasConfigurationType() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.adasConfigurationType();
    }

    /**
     * rearSeatTouchPanelFuncEnable
     * @return boolean
     */
    public boolean rearSeatTouchPanelFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.rearSeatTouchPanelFuncEnable();
    }

    /**
     * hudFuncEnable
     * @return hudFuncEnable
     */
    public int hudFuncEnable() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.hudFuncEnable();
    }

    /**
     * navigationDeflectionEnable
     * @return boolean
     */
    public boolean navigationDeflectionEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.navigationDeflectionEnable();
    }

    /**
     * architecture
     * @return int
     */
    public int architecture() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.architecture();
    }

    /**
     * navigationPreConditionDataProvideEnable
     * @return boolean
     */
    public boolean navigationPreConditionDataProvideEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.navigationPreConditionDataProvideEnable();
    }

    /**
     * navigaitonSupplier
     * @return int
     */
    public int navigaitonSupplier() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.navigaitonSupplier();
    }

    /**
     * highVoltageBatteryPropulsionTotalRangeNavi
     * @return int
     */
    public int highVoltageBatteryPropulsionTotalRangeNavi() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.highVoltageBatteryPropulsionTotalRangeNavi();
    }

    /**
     * poiSearchFuncEnable
     * @return boolean
     */
    public boolean poiSearchFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.poiSearchFuncEnable();
    }

    /**
     * scenarioEngineFuncEnable
     * @return boolean
     */
    public boolean scenarioEngineFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.scenarioEngineFuncEnable();
    }

    /**
     * globalSearchFuncEnable
     * @return boolean
     */
    public boolean globalSearchFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.globalSearchFuncEnable();
    }

    /**
     * teamTravelFuncEnable
     * @return boolean
     */
    public boolean teamTravelFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.teamTravelFuncEnable();
    }

    /**
     * bootAnimationReplacementFuncEnable
     * @return boolean
     */
    public boolean bootAnimationReplacementFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.bootAnimationReplacementFuncEnable();
    }

    /**
     * imeFuncEnable
     * @return boolean
     */
    public boolean imeFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.imeFuncEnable();
    }

    /**
     * wallpaperThemeFuncEnable
     * @return boolean
     */
    public boolean wallpaperThemeFuncEnable() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.wallpaperThemeFuncEnable();
    }

    /**
     * themeDefaultValue
     * @return int
     */
    public int themeDefaultValue() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.themeDefaultValue();
    }

    /**
     * slopeUpCostlist
     * @return float[]
     */
    public float[] slopeUpCostlist() {
        if (mCalibrationApi == null) {
            return null;
        }
        return mCalibrationApi.slopeUpCostlist();
    }

    /**
     * slopeDownCostlist
     * @return float[]
     */
    public float[] slopeDownCostlist() {
        if (mCalibrationApi == null) {
            return null;
        }
        return mCalibrationApi.slopeDownCostlist();
    }

    /**
     * transAccessCostlist
     * @return float[]
     */
    public float[] transAccessCostlist() {
        if (mCalibrationApi == null) {
            return null;
        }
        return mCalibrationApi.transAccessCostlist();
    }

    /**
     * transDecessCostlist
     * @return float[]
     */
    public float[] transDecessCostlist() {
        if (mCalibrationApi == null) {
            return null;
        }
        return mCalibrationApi.transDecessCostlist();
    }

    /**
     * speedCostlist
     * @return Map
     */
    public Map<Integer, Float> speedCostlist() {
        if (mCalibrationApi == null) {
            return null;
        }
        return mCalibrationApi.speedCostlist();
    }

    /**
     * auxCostlist
     * @return float[]
     */
    public float[] auxCostlist() {
        if (mCalibrationApi == null) {
            return null;
        }
        return mCalibrationApi.auxCostlist();
    }

    /**
     * vehicleWeight
     * @return int
     */
    public int vehicleWeight() {
        if (mCalibrationApi == null) {
            return -1;
        }
        return mCalibrationApi.vehicleWeight();
    }

    public String getDeviceId() {
        if (mCalibrationApi == null) {
            return "";
        }
        return mCalibrationApi.getDeviceId();
    }

    public boolean getHudSnowMode() {
        if (mCalibrationApi == null) {
            return false;
        }
        return mCalibrationApi.getHudSnowMode();
    }
}
