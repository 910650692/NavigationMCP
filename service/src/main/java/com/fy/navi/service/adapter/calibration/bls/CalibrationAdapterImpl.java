package com.fy.navi.service.adapter.calibration.bls;

import android.util.Log;

import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.calibration.CalibrationApi;

import java.util.Arrays;
import java.util.Map;

import gm.calibrations.CalId;
import gm.calibrations.CalibrationManager;

public class CalibrationAdapterImpl implements CalibrationApi {
    private static final String TAG = MapDefaultFinalTag.CALIBRATION_SERVICE_TAG;

    private final CalibrationManager mCalibrationManager;

    public CalibrationAdapterImpl() {
        mCalibrationManager = new CalibrationManager();
    }

    @Override
    public int powerType() {
        final boolean isPetrol = mCalibrationManager.getBoolean(CalId.VEHICLE_FUEL_TYPE_PETROL, false);
        final boolean isElectric = mCalibrationManager.getBoolean(CalId.VEHICLE_FUEL_TYPE_ELECTRIC, false);
        final boolean isHybrid = mCalibrationManager.getBoolean(CalId.VEHICLE_FUEL_TYPE_HYBRID, false);
        Log.d(TAG, "s powerType: isPetrol = " + isPetrol + ", isElectric = " + isElectric + ", isHybrid = " + isHybrid);
        if (isPetrol && !isElectric && !isHybrid) {
            return 0;
        } else if (!isPetrol && isElectric && !isHybrid) {
            return 1;
        } else if (!isPetrol && !isElectric && isHybrid) {
            return 2;
        } else {
            return -1;
        }
    }

    @Override
    public int brand() {
        final int integer = mCalibrationManager.getEnumeration(CalId.GMBrand, -1);
        Log.d(TAG, "s GMBrand: " + integer);
        return integer;
    }

    @Override
    public int model() {
        final int integer = mCalibrationManager.getEnumeration(CalId.GMModel, -1);
        Log.d(TAG, "s GMModel: " + integer);
        return integer;
    }

    @Override
    public boolean enableApplicationNavigation() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.ENABLE_APPLICATION_NAVIGATION, true);
        Log.d(TAG, "s ENABLE_APPLICATION_NAVIGATION: " + enable);
        return enable;
    }

    @Override
    public boolean laneLevelNavigatioFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_LaneLevelNavigation_Func_Enable, false);
        Log.d(TAG, "s P_LaneLevelNavigation_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public boolean v2xMapDisplayFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_V2XMapDisplay_Func_Enable, false);
        Log.d(TAG, "s P_V2XMapDisplay_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public int speedLimitInformationSource() {
        final int integer = mCalibrationManager.getEnumeration(CalId.P_SpeedLimitInformationSource, -1);
        Log.d(TAG, "s P_SpeedLimitInformationSource: " + integer);
        return integer;
    }

    @Override
    public int adasConfigurationInfomation() {
        final int integer = mCalibrationManager.getEnumeration(CalId.P_ADAS_Configuration_Infomation, -1);
        Log.d(TAG, "s P_ADAS_Configuration_Infomation: " + integer);
        return integer;
    }

    @Override
    public int adasConfigurationType() {
        final int integer = mCalibrationManager.getEnumeration(CalId.ADAS_Configuration_Type, -1);
        Log.d(TAG, "s ADAS_Configuration_Type: " + integer);
        return integer;
    }

    @Override
    public boolean rearSeatTouchPanelFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_RearSeatTouchPanel_Func_Enable, false);
        Log.d(TAG, "s P_RearSeatTouchPanel_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public int hudFuncEnable() {
        final int integer = mCalibrationManager.getEnumeration(CalId.P_HUD_Func_Enable, -1);
        Log.d(TAG, "s P_HUD_Func_Enable: " + integer);
        return integer;
    }

    @Override
    public boolean navigationDeflectionEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_Navigation_Deflection_Enable, false);
        Log.d(TAG, "s P_Navigation_Deflection_Enable: " + enable);
        return enable;
    }

    @Override
    public int architecture() {
        final int integer = mCalibrationManager.getEnumeration(CalId.P_ARCHITECTURE, -1);
        Log.d(TAG, "s P_ARCHITECTURE: " + integer);
        return integer;
    }

    @Override
    public boolean navigationPreConditionDataProvideEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_Navigation_PreCondition_DataProvide_Enable, true);
        Log.d(TAG, "s P_Navigation_PreCondition_DataProvide_Enable: " + enable);
        return enable;
    }

    @Override
    public int navigaitonSupplier() {
        final int integer = mCalibrationManager.getEnumeration(CalId.P_Navigaiton_Supplier, -1);
        Log.d(TAG, "s P_Navigaiton_Supplier: " + integer);
        return integer;
    }

    @Override
    public int highVoltageBatteryPropulsionTotalRangeNavi() {
        final int integer = mCalibrationManager.getInteger(CalId.P_High_Voltage_Battery_Propulsion_Total_Range_Navi, -1);
        Log.d(TAG, "s P_High_Voltage_Battery_Propulsion_Total_Range_Navi: " + integer);
        return integer;
    }

    @Override
    public boolean poiSearchFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_POISearch_Func_Enable, true);
        Log.d(TAG, "s P_POISearch_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public boolean scenarioEngineFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_ScenarioEngine_Func_Enable, true);
        Log.d(TAG, "s P_ScenarioEngine_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public boolean globalSearchFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_GlobalSearch_Func_Enable, true);
        Log.d(TAG, "s P_GlobalSearch_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public boolean teamTravelFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_TeamTravel_Func_Enable, true);
        Log.d(TAG, "s P_TeamTravel_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public boolean bootAnimationReplacementFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_BootAnimationReplacement_Func_Enable, true);
        Log.d(TAG, "s P_BootAnimationReplacement_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public boolean imeFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_IME_Func_Enable, true);
        Log.d(TAG, "s P_IME_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public boolean wallpaperThemeFuncEnable() {
        final boolean enable = mCalibrationManager.getBoolean(CalId.P_Wallpaper_Theme_Func_Enable, true);
        Log.d(TAG, "s P_Wallpaper_Theme_Func_Enable: " + enable);
        return enable;
    }

    @Override
    public int themeDefaultValue() {
        final int integer = mCalibrationManager.getEnumeration(CalId.P_THEME_DEFAULT_VALUE, -1);
        Log.d(TAG, "s P_THEME_DEFAULT_VALUE: " + integer);
        return integer;
    }

    @Override
    public float[] slopeUpCostlist() {
        final float[] array = mCalibrationManager.getFloatArray("SLOPE_UP_COSTLIST");
        Log.d(TAG, "s SLOPE_UP_COSTLIST: " + Arrays.toString(array));
        return array;
    }

    @Override
    public float[] slopeDownCostlist() {
        final float[] array = mCalibrationManager.getFloatArray("SLOPE_DOWN_COSTLIST");
        Log.d(TAG, "s SLOPE_DOWN_COSTLIST: " + Arrays.toString(array));
        return array;
    }

    @Override
    public float[] transAccessCostlist() {
        final float[] array = mCalibrationManager.getFloatArray("TRANS_ACCESS_COSTLIST");
        Log.d(TAG, "s TRANS_ACCESS_COSTLIST: " + Arrays.toString(array));
        return array;
    }

    @Override
    public float[] transDecessCostlist() {
        final float[] array = mCalibrationManager.getFloatArray("TRANS_DECESS_COSTLIST");
        Log.d(TAG, "s TRANS_DECESS_COSTLIST: " + Arrays.toString(array));
        return array;
    }

    @Override
    public Map<Integer, Float> speedCostlist() {
        final Map<Integer, Float> map = mCalibrationManager.getEnumFloatMap("SPEED_COSTLIST");
        Log.d(TAG, "s SPEED_COSTLIST: " + map);
        return map;
    }

    @Override
    public float[] auxCostlist() {
        final float[] array = mCalibrationManager.getFloatArray("AUX_COSTLIST");
        Log.d(TAG, "s AUX_COSTLIST: " + Arrays.toString(array));
        return array;
    }

    @Override
    public int vehicleWeight() {
        final int integer = mCalibrationManager.getInteger("VEHICLE_WEIGHT", -1);
        Log.d(TAG, "s VEHICLE_WEIGHT: " + integer);
        return integer;
    }
}
