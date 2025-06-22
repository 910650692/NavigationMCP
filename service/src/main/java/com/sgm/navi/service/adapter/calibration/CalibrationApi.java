package com.sgm.navi.service.adapter.calibration;

import java.util.Map;

public interface CalibrationApi {

    void init();

    /**
     * powerType
     * @return int
     */
    int powerType();

    /**
     * brand
     * @return int
     */
    int brand();

    /**
     * model
     * @return int
     */
    int model();

    /**
     * enableApplicationNavigation
     * @return boolean
     */
    boolean enableApplicationNavigation();

    /**
     * laneLevelNavigatioFuncEnable
     * @return boolean
     */
    boolean laneLevelNavigatioFuncEnable();

    /**
     * v2xMapDisplayFuncEnable
     * @return boolean
     */
    boolean v2xMapDisplayFuncEnable();

    /**
     * speedLimitInformationSource
     * @return int
     */
    int speedLimitInformationSource();

    /**
     * adasConfigurationInfomation
     * @return int
     */
    int adasConfigurationInfomation();

    /**
     * adasConfigurationType
     * @return int
     */
    int adasConfigurationType();

    /**
     * rearSeatTouchPanelFuncEnable
     * @return boolean
     */
    boolean rearSeatTouchPanelFuncEnable();

    /**
     * hudFuncEnable
     * @return hudFuncEnable
     */
    int hudFuncEnable();

    /**
     * navigationDeflectionEnable
     * @return boolean
     */
    boolean navigationDeflectionEnable();

    /**
     * architecture
     * @return int
     */
    int architecture();

    /**
     * navigationPreConditionDataProvideEnable
     * @return boolean
     */
    boolean navigationPreConditionDataProvideEnable();

    /**
     * navigaitonSupplier
     * @return int
     */
    int navigaitonSupplier();

    /**
     * highVoltageBatteryPropulsionTotalRangeNavi
     * @return int
     */
    int highVoltageBatteryPropulsionTotalRangeNavi();

    /**
     * poiSearchFuncEnable
     * @return boolean
     */
    boolean poiSearchFuncEnable();

    /**
     * scenarioEngineFuncEnable
     * @return boolean
     */
    boolean scenarioEngineFuncEnable();

    /**
     * globalSearchFuncEnable
     * @return boolean
     */
    boolean globalSearchFuncEnable();

    /**
     * teamTravelFuncEnable
     * @return boolean
     */
    boolean teamTravelFuncEnable();

    /**
     * bootAnimationReplacementFuncEnable
     * @return boolean
     */
    boolean bootAnimationReplacementFuncEnable();

    /**
     * imeFuncEnable
     * @return boolean
     */
    boolean imeFuncEnable();

    /**
     * wallpaperThemeFuncEnable
     * @return boolean
     */
    boolean wallpaperThemeFuncEnable();

    /**
     * themeDefaultValue
     * @return int
     */
    int themeDefaultValue();

    /**
     * slopeUpCostlist
     * @return float[]
     */
    float[] slopeUpCostlist();

    /**
     * slopeDownCostlist
     * @return float[]
     */
    float[] slopeDownCostlist();

    /**
     * transAccessCostlist
     * @return float[]
     */
    float[] transAccessCostlist();

    /**
     * transDecessCostlist
     * @return float[]
     */
    float[] transDecessCostlist();

    /**
     * speedCostlist
     * @return Map
     */
    Map<Integer, Float> speedCostlist();

    /**
     * auxCostlist
     * @return float[]
     */
    float[] auxCostlist();

    /**
     * vehicleWeight
     * @return int
     */
    int vehicleWeight();
}
