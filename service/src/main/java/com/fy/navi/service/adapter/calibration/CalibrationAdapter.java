package com.fy.navi.service.adapter.calibration;

import com.fy.navi.service.AdapterConfig;

import java.util.Map;
import java.util.Objects;

public final class CalibrationAdapter {
    private static final String CALIBRATION_API_PKG = Objects.requireNonNull(CalibrationAdapter.class.getPackage()).getName();
    private static final String CALIBRATION_API_CLS = "CalibrationAdapterImpl";
    private final CalibrationApi mCalibrationApi;

    public static CalibrationAdapter getInstance() {
        return CalibrationAdapter.SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final CalibrationAdapter INSTANCE = new CalibrationAdapter();
    }

    private CalibrationAdapter() {
        mCalibrationApi = (CalibrationApi) AdapterConfig.getObject(CALIBRATION_API_PKG, CALIBRATION_API_CLS);
    }

    public void init() {
        mCalibrationApi.init();
    }

    /**
     * powerType
     * @return int
     */
    public int powerType() {
        return mCalibrationApi.powerType();
    }

    /**
     * brand
     * @return int
     */
    public int brand() {
        return mCalibrationApi.brand();
    }

    /**
     * model
     * @return int
     */
    public int model() {
        return mCalibrationApi.model();
    }

    /**
     * enableApplicationNavigation
     * @return boolean
     */
    public boolean enableApplicationNavigation() {
        return mCalibrationApi.enableApplicationNavigation();
    }

    /**
     * laneLevelNavigatioFuncEnable
     * @return boolean
     */
    public boolean laneLevelNavigatioFuncEnable() {
        return mCalibrationApi.laneLevelNavigatioFuncEnable();
    }

    /**
     * v2xMapDisplayFuncEnable
     * @return boolean
     */
    public boolean v2xMapDisplayFuncEnable() {
        return mCalibrationApi.v2xMapDisplayFuncEnable();
    }

    /**
     * speedLimitInformationSource
     * @return int
     */
    public int speedLimitInformationSource() {
        return mCalibrationApi.speedLimitInformationSource();
    }

    /**
     * adasConfigurationInfomation
     * @return int
     */
    public int adasConfigurationInfomation() {
        return mCalibrationApi.adasConfigurationInfomation();
    }

    /**
     * adasConfigurationType
     * @return int
     */
    public int adasConfigurationType() {
        return mCalibrationApi.adasConfigurationType();
    }

    /**
     * rearSeatTouchPanelFuncEnable
     * @return boolean
     */
    public boolean rearSeatTouchPanelFuncEnable() {
        return mCalibrationApi.rearSeatTouchPanelFuncEnable();
    }

    /**
     * hudFuncEnable
     * @return hudFuncEnable
     */
    public int hudFuncEnable() {
        return mCalibrationApi.hudFuncEnable();
    }

    /**
     * navigationDeflectionEnable
     * @return boolean
     */
    public boolean navigationDeflectionEnable() {
        return mCalibrationApi.navigationDeflectionEnable();
    }

    /**
     * architecture
     * @return int
     */
    public int architecture() {
        return mCalibrationApi.architecture();
    }

    /**
     * navigationPreConditionDataProvideEnable
     * @return boolean
     */
    public boolean navigationPreConditionDataProvideEnable() {
        return mCalibrationApi.navigationPreConditionDataProvideEnable();
    }

    /**
     * navigaitonSupplier
     * @return int
     */
    public int navigaitonSupplier() {
        return mCalibrationApi.navigaitonSupplier();
    }

    /**
     * highVoltageBatteryPropulsionTotalRangeNavi
     * @return int
     */
    public int highVoltageBatteryPropulsionTotalRangeNavi() {
        return mCalibrationApi.highVoltageBatteryPropulsionTotalRangeNavi();
    }

    /**
     * poiSearchFuncEnable
     * @return boolean
     */
    public boolean poiSearchFuncEnable() {
        return mCalibrationApi.poiSearchFuncEnable();
    }

    /**
     * scenarioEngineFuncEnable
     * @return boolean
     */
    public boolean scenarioEngineFuncEnable() {
        return mCalibrationApi.scenarioEngineFuncEnable();
    }

    /**
     * globalSearchFuncEnable
     * @return boolean
     */
    public boolean globalSearchFuncEnable() {
        return mCalibrationApi.globalSearchFuncEnable();
    }

    /**
     * teamTravelFuncEnable
     * @return boolean
     */
    public boolean teamTravelFuncEnable() {
        return mCalibrationApi.teamTravelFuncEnable();
    }

    /**
     * bootAnimationReplacementFuncEnable
     * @return boolean
     */
    public boolean bootAnimationReplacementFuncEnable() {
        return mCalibrationApi.bootAnimationReplacementFuncEnable();
    }

    /**
     * imeFuncEnable
     * @return boolean
     */
    public boolean imeFuncEnable() {
        return mCalibrationApi.imeFuncEnable();
    }

    /**
     * wallpaperThemeFuncEnable
     * @return boolean
     */
    public boolean wallpaperThemeFuncEnable() {
        return mCalibrationApi.wallpaperThemeFuncEnable();
    }

    /**
     * themeDefaultValue
     * @return int
     */
    public int themeDefaultValue() {
        return mCalibrationApi.themeDefaultValue();
    }

    /**
     * slopeUpCostlist
     * @return float[]
     */
    public float[] slopeUpCostlist() {
        return mCalibrationApi.slopeUpCostlist();
    }

    /**
     * slopeDownCostlist
     * @return float[]
     */
    public float[] slopeDownCostlist() {
        return mCalibrationApi.slopeDownCostlist();
    }

    /**
     * transAccessCostlist
     * @return float[]
     */
    public float[] transAccessCostlist() {
        return mCalibrationApi.transAccessCostlist();
    }

    /**
     * transDecessCostlist
     * @return float[]
     */
    public float[] transDecessCostlist() {
        return mCalibrationApi.transDecessCostlist();
    }

    /**
     * speedCostlist
     * @return Map
     */
    public Map<Integer, Float> speedCostlist() {
        return mCalibrationApi.speedCostlist();
    }

    /**
     * auxCostlist
     * @return float[]
     */
    public float[] auxCostlist() {
        return mCalibrationApi.auxCostlist();
    }

    /**
     * vehicleWeight
     * @return int
     */
    public int vehicleWeight() {
        return mCalibrationApi.vehicleWeight();
    }
}
