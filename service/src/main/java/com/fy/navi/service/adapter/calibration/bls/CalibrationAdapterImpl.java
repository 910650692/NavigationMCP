package com.fy.navi.service.adapter.calibration.bls;

import android.util.Log;

import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.calibration.CalibrationApi;

import java.util.Arrays;
import java.util.HashMap;
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
        try {
            boolean isPetrol = mCalibrationManager.getBoolean(CalId.VEHICLE_FUEL_TYPE_PETROL, false);
            boolean isElectric = mCalibrationManager.getBoolean(CalId.VEHICLE_FUEL_TYPE_ELECTRIC, false);
            boolean isHybrid = mCalibrationManager.getBoolean(CalId.VEHICLE_FUEL_TYPE_HYBRID, false);
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
        } catch (Exception e) {
            Log.e(TAG, "f powerType: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public int brand() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.GMBrand, -1);
            Log.d(TAG, "s GMBrand: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f GMBrand: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public int model() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.GMModel, -1);
            Log.d(TAG, "s GMModel: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f GMModel: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public boolean enableApplicationNavigation() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.ENABLE_APPLICATION_NAVIGATION, true);
            Log.d(TAG, "s ENABLE_APPLICATION_NAVIGATION: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f ENABLE_APPLICATION_NAVIGATION: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public boolean laneLevelNavigatioFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_LaneLevelNavigation_Func_Enable, false);
            Log.d(TAG, "s P_LaneLevelNavigation_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_LaneLevelNavigation_Func_Enable: " + Log.getStackTraceString(e));
            return false;
        }
    }

    @Override
    public boolean V2XMapDisplayFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_V2XMapDisplay_Func_Enable, false);
            Log.d(TAG, "s P_V2XMapDisplay_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_V2XMapDisplay_Func_Enable: " + Log.getStackTraceString(e));
            return false;
        }
    }

    @Override
    public int speedLimitInformationSource() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.P_SpeedLimitInformationSource, -1);
            Log.d(TAG, "s P_SpeedLimitInformationSource: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f P_SpeedLimitInformationSource: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public int ADASConfigurationInfomation() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.P_ADAS_Configuration_Infomation, -1);
            Log.d(TAG, "s P_ADAS_Configuration_Infomation: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f P_ADAS_Configuration_Infomation: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public int ADASConfigurationType() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.ADAS_Configuration_Type, -1);
            Log.d(TAG, "s ADAS_Configuration_Type: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f ADAS_Configuration_Type: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public boolean rearSeatTouchPanelFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_RearSeatTouchPanel_Func_Enable, false);
            Log.d(TAG, "s P_RearSeatTouchPanel_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_RearSeatTouchPanel_Func_Enable: " + Log.getStackTraceString(e));
            return false;
        }
    }

    @Override
    public int HUDFuncEnable() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.P_HUD_Func_Enable, -1);
            Log.d(TAG, "s P_HUD_Func_Enable: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f P_HUD_Func_Enable: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public boolean navigationDeflectionEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_Navigation_Deflection_Enable, false);
            Log.d(TAG, "s P_Navigation_Deflection_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_Navigation_Deflection_Enable: " + Log.getStackTraceString(e));
            return false;
        }
    }

    @Override
    public int architecture() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.P_ARCHITECTURE, -1);
            Log.d(TAG, "s P_ARCHITECTURE: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f P_ARCHITECTURE: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public boolean navigationPreConditionDataProvideEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_Navigation_PreCondition_DataProvide_Enable, true);
            Log.d(TAG, "s P_Navigation_PreCondition_DataProvide_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_Navigation_PreCondition_DataProvide_Enable: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public int navigaitonSupplier() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.P_Navigaiton_Supplier, -1);
            Log.d(TAG, "s P_Navigaiton_Supplier: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f P_Navigaiton_Supplier: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public int highVoltageBatteryPropulsionTotalRangeNavi() {
        try {
            int integer = mCalibrationManager.getInteger(CalId.P_High_Voltage_Battery_Propulsion_Total_Range_Navi, -1);
            Log.d(TAG, "s P_High_Voltage_Battery_Propulsion_Total_Range_Navi: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f P_High_Voltage_Battery_Propulsion_Total_Range_Navi: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public boolean POISearchFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_POISearch_Func_Enable, true);
            Log.d(TAG, "s P_POISearch_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_POISearch_Func_Enable: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public boolean scenarioEngineFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_ScenarioEngine_Func_Enable, true);
            Log.d(TAG, "s P_ScenarioEngine_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_ScenarioEngine_Func_Enable: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public boolean globalSearchFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_GlobalSearch_Func_Enable, true);
            Log.d(TAG, "s P_GlobalSearch_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_GlobalSearch_Func_Enable: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public boolean teamTravelFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_TeamTravel_Func_Enable, true);
            Log.d(TAG, "s P_TeamTravel_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_TeamTravel_Func_Enable: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public boolean bootAnimationReplacementFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_BootAnimationReplacement_Func_Enable, true);
            Log.d(TAG, "s P_BootAnimationReplacement_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_BootAnimationReplacement_Func_Enable: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public boolean IMEFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_IME_Func_Enable, true);
            Log.d(TAG, "s P_IME_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_IME_Func_Enable: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public boolean wallpaperThemeFuncEnable() {
        try {
            boolean enable = mCalibrationManager.getBoolean(CalId.P_Wallpaper_Theme_Func_Enable, true);
            Log.d(TAG, "s P_Wallpaper_Theme_Func_Enable: " + enable);
            return enable;
        } catch (Exception e) {
            Log.e(TAG, "f P_Wallpaper_Theme_Func_Enable: " + Log.getStackTraceString(e));
            return true;
        }
    }

    @Override
    public int themeDefaultValue() {
        try {
            int integer = mCalibrationManager.getEnumeration(CalId.P_THEME_DEFAULT_VALUE, -1);
            Log.d(TAG, "s P_THEME_DEFAULT_VALUE: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f P_THEME_DEFAULT_VALUE: " + Log.getStackTraceString(e));
            return -1;
        }
    }

    @Override
    public float[] slopeUpCostlist() {
        try {
            float[] array = mCalibrationManager.getFloatArray("SLOPE_UP_COSTLIST");
            Log.d(TAG, "s SLOPE_UP_COSTLIST: " + Arrays.toString(array));
            return array;
        } catch (Exception e) {
            Log.e(TAG, "f SLOPE_UP_COSTLIST: " + Log.getStackTraceString(e));
            return new float[0];
        }
    }

    @Override
    public float[] slopeDownCostlist() {
        try {
            float[] array = mCalibrationManager.getFloatArray("SLOPE_DOWN_COSTLIST");
            Log.d(TAG, "s SLOPE_DOWN_COSTLIST: " + Arrays.toString(array));
            return array;
        } catch (Exception e) {
            Log.e(TAG, "f SLOPE_DOWN_COSTLIST: " + Log.getStackTraceString(e));
            return new float[0];
        }
    }

    @Override
    public float[] transAccessCostlist() {
        try {
            float[] array = mCalibrationManager.getFloatArray("TRANS_ACCESS_COSTLIST");
            Log.d(TAG, "s TRANS_ACCESS_COSTLIST: " + Arrays.toString(array));
            return array;
        } catch (Exception e) {
            Log.e(TAG, "f TRANS_ACCESS_COSTLIST: " + Log.getStackTraceString(e));
            return new float[0];
        }
    }

    @Override
    public float[] transDecessCostlist() {
        try {
            float[] array = mCalibrationManager.getFloatArray("TRANS_DECESS_COSTLIST");
            Log.d(TAG, "s TRANS_DECESS_COSTLIST: " + Arrays.toString(array));
            return array;
        } catch (Exception e) {
            Log.e(TAG, "f TRANS_DECESS_COSTLIST: " + Log.getStackTraceString(e));
            return new float[0];
        }
    }

    @Override
    public Map<Integer, Float> speedCostlist() {
        try {
            Map<Integer, Float> map = mCalibrationManager.getEnumFloatMap("SPEED_COSTLIST");
            Log.d(TAG, "s SPEED_COSTLIST: " + map);
            return map;
        } catch (Exception e) {
            Log.e(TAG, "f SPEED_COSTLIST: " + Log.getStackTraceString(e));
            return new HashMap<>();
        }
    }

    @Override
    public float[] auxCostlist() {
        try {
            float[] array = mCalibrationManager.getFloatArray("AUX_COSTLIST");
            Log.d(TAG, "s AUX_COSTLIST: " + Arrays.toString(array));
            return array;
        } catch (Exception e) {
            Log.e(TAG, "f AUX_COSTLIST: " + Log.getStackTraceString(e));
            return new float[0];
        }
    }

    @Override
    public int vehicleWeight() {
        try {
            int integer = mCalibrationManager.getInteger("VEHICLE_WEIGHT", -1);
            Log.d(TAG, "s VEHICLE_WEIGHT: " + integer);
            return integer;
        } catch (Exception e) {
            Log.e(TAG, "f VEHICLE_WEIGHT: " + Log.getStackTraceString(e));
            return -1;
        }
    }
}
