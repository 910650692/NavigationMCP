package com.fy.navi.service.adapter.calibration;

import java.util.Map;

public interface CalibrationApi {

    int powerType();

    int brand();

    int model();

    boolean enableApplicationNavigation();

    boolean laneLevelNavigatioFuncEnable();

    boolean V2XMapDisplayFuncEnable();

    int speedLimitInformationSource();

    int ADASConfigurationInfomation();

    int ADASConfigurationType();

    boolean rearSeatTouchPanelFuncEnable();

    int HUDFuncEnable();

    boolean navigationDeflectionEnable();

    int architecture();

    boolean navigationPreConditionDataProvideEnable();

    int navigaitonSupplier();

    int highVoltageBatteryPropulsionTotalRangeNavi();

    boolean POISearchFuncEnable();

    boolean scenarioEngineFuncEnable();

    boolean globalSearchFuncEnable();

    boolean teamTravelFuncEnable();

    boolean bootAnimationReplacementFuncEnable();

    boolean IMEFuncEnable();

    boolean wallpaperThemeFuncEnable();

    int themeDefaultValue();

    float[] slopeUpCostlist();

    float[] slopeDownCostlist();

    float[] transAccessCostlist();

    float[] transDecessCostlist();

    Map<Integer, Float> speedCostlist();

    float[] auxCostlist();

    int vehicleWeight();
}
