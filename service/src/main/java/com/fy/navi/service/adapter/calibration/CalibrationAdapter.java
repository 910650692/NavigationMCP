package com.fy.navi.service.adapter.calibration;

import com.fy.navi.service.AdapterConfig;

import java.util.Map;
import java.util.Objects;

public class CalibrationAdapter {
    private static final String CALIBRATION_API_PKG = Objects.requireNonNull(CalibrationAdapter.class.getPackage()).getName();
    private static final String CALIBRATION_API_CLS = "CalibrationAdapterImpl";
    private final CalibrationApi mCalibrationApi;

    public static CalibrationAdapter getInstance() {
        return CalibrationAdapter.SInstanceHolder.sInstance;
    }

    private static final class SInstanceHolder {
        static final CalibrationAdapter sInstance = new CalibrationAdapter();
    }

    private CalibrationAdapter() {
        mCalibrationApi = (CalibrationApi) AdapterConfig.getObject(CALIBRATION_API_PKG, CALIBRATION_API_CLS);
    }

    public int powerType() {
        return mCalibrationApi.powerType();
    }

    public int brand() {
        return mCalibrationApi.brand();
    }

    public int model() {
        return mCalibrationApi.model();
    }

    public boolean enableApplicationNavigation() {
        return mCalibrationApi.enableApplicationNavigation();
    }

    public boolean laneLevelNavigatioFuncEnable() {
        return mCalibrationApi.laneLevelNavigatioFuncEnable();
    }

    public boolean V2XMapDisplayFuncEnable() {
        return mCalibrationApi.V2XMapDisplayFuncEnable();
    }

    public int speedLimitInformationSource() {
        return mCalibrationApi.speedLimitInformationSource();
    }

    public int ADASConfigurationInfomation() {
        return mCalibrationApi.ADASConfigurationInfomation();
    }

    public int ADASConfigurationType() {
        return mCalibrationApi.ADASConfigurationType();
    }

    public boolean rearSeatTouchPanelFuncEnable() {
        return mCalibrationApi.rearSeatTouchPanelFuncEnable();
    }

    public int HUDFuncEnable() {
        return mCalibrationApi.HUDFuncEnable();
    }

    public boolean navigationDeflectionEnable() {
        return mCalibrationApi.navigationDeflectionEnable();
    }

    public int architecture() {
        return mCalibrationApi.architecture();
    }

    public boolean navigationPreConditionDataProvideEnable() {
        return mCalibrationApi.navigationPreConditionDataProvideEnable();
    }

    public int navigaitonSupplier() {
        return mCalibrationApi.navigaitonSupplier();
    }

    public int highVoltageBatteryPropulsionTotalRangeNavi() {
        return mCalibrationApi.highVoltageBatteryPropulsionTotalRangeNavi();
    }

    public boolean POISearchFuncEnable() {
        return mCalibrationApi.POISearchFuncEnable();
    }

    public boolean scenarioEngineFuncEnable() {
        return mCalibrationApi.scenarioEngineFuncEnable();
    }

    public boolean globalSearchFuncEnable() {
        return mCalibrationApi.globalSearchFuncEnable();
    }

    public boolean teamTravelFuncEnable() {
        return mCalibrationApi.teamTravelFuncEnable();
    }

    public boolean bootAnimationReplacementFuncEnable() {
        return mCalibrationApi.bootAnimationReplacementFuncEnable();
    }

    public boolean IMEFuncEnable() {
        return mCalibrationApi.IMEFuncEnable();
    }

    public boolean wallpaperThemeFuncEnable() {
        return mCalibrationApi.wallpaperThemeFuncEnable();
    }

    public int themeDefaultValue() {
        return mCalibrationApi.themeDefaultValue();
    }

    public float[] slopeUpCostlist() {
        return mCalibrationApi.slopeUpCostlist();
    }

    public float[] slopeDownCostlist() {
        return mCalibrationApi.slopeDownCostlist();
    }

    public float[] transAccessCostlist() {
        return mCalibrationApi.transAccessCostlist();
    }

    public float[] transDecessCostlist() {
        return mCalibrationApi.transDecessCostlist();
    }

    public Map<Integer, Float> speedCostlist() {
        return mCalibrationApi.speedCostlist();
    }

    public float[] auxCostlist() {
        return mCalibrationApi.auxCostlist();
    }

    public int vehicleWeight() {
        return mCalibrationApi.vehicleWeight();
    }
}
