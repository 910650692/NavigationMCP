package com.sgm.navi.service.adapter.calibration.bls;

import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.calibration.CalibrationAdapterCallback;
import com.sgm.navi.service.adapter.calibration.CalibrationApi;

import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import gm.calibrations.CalId;
import gm.calibrations.CalibrationManager;
import patac.manager.PatacServiceManager;
import patac.manager.PatacServiceNotConnectedException;
import patac.manager.setting.PatacRemoteSettingsManager;
import patac.manager.vehicle.PatacVehicleManager;

public class CalibrationAdapterImpl implements CalibrationApi {
    private static final String TAG = MapDefaultFinalTag.CALIBRATION_SERVICE_TAG;

    private ConcurrentHashMap<String, CalibrationAdapterCallback> mCallback = new ConcurrentHashMap<>();

    private CalibrationManager mCalibrationManager;
    private PatacServiceManager mPatacServiceManager;
    private PatacVehicleManager mPatacVehicleManager;
    private PatacRemoteSettingsManager mPatacRemoteSettingsManager;
    private boolean mSnowMode;

    private int powerType;
    private int vehiclePropulsionType = -1;
    private int GMBrand;
    private int GMModel;
    private boolean ENABLE_APPLICATION_NAVIGATION;
    private boolean P_LaneLevelNavigation_Func_Enable;
    private boolean P_V2XMapDisplay_Func_Enable;
    private int P_SpeedLimitInformationSource;
    private int P_ADAS_Configuration_Infomation;
    private int ADAS_Configuration_Type = -1;
    private boolean P_RearSeatTouchPanel_Func_Enable;
    private int P_HUD_Func_Enable;
    private boolean P_Navigation_Deflection_Enable;
    private int P_ARCHITECTURE;
    private boolean P_Navigation_PreCondition_DataProvide_Enable;
    private int P_Navigaiton_Supplier;
    private int P_High_Voltage_Battery_Propulsion_Total_Range_Navi;
    private boolean P_POISearch_Func_Enable;
    private boolean P_ScenarioEngine_Func_Enable;
    private boolean P_GlobalSearch_Func_Enable;
    private boolean P_TeamTravel_Func_Enable;
    private boolean P_BootAnimationReplacement_Func_Enable;
    private boolean P_IME_Func_Enable;
    private boolean P_Wallpaper_Theme_Func_Enable;
    private int P_THEME_DEFAULT_VALUE;
    private float[] SLOPE_UP_COSTLIST;
    private float[] SLOPE_DOWN_COSTLIST;
    private float[] TRANS_ACCESS_COSTLIST;
    private float[] TRANS_DECESS_COSTLIST;
    private Map<Integer, Float> SPEED_COSTLIST;
    private float[] AUX_COSTLIST;
    private int VEHICLE_WEIGHT;

    public CalibrationAdapterImpl() {
    }

    @Override
    public void init() {
        Logger.d(TAG, "init start");
        mCalibrationManager = new CalibrationManager();
        if (Objects.equals(BuildConfig.FLAVOR, "cadi") || Objects.equals(BuildConfig.FLAVOR, "buick")) {
            vehiclePropulsionType = mCalibrationManager.getEnumeration("KeOCD_int_VEHICLE_PROPULSION_TYPE", -1);
        } else {
            vehiclePropulsionType = mCalibrationManager.getEnumeration("P_VEHICLE_PROPULSION_TYPE", -1);
        }
        Logger.d(TAG, "s vehiclePropulsionType: ", vehiclePropulsionType);
        switch (vehiclePropulsionType) {
            case 0: // CeOCD_VehPrplsnType_Gas
                vehiclePropulsionType = 0;
                powerType = 0;
                break;
            case 3: // CeOCD_VehPrplsnType_BEV
                vehiclePropulsionType = 1;
                powerType = 1;
                break;
            case 5: // CeOCD_VehPrplsnType_PHEV
                vehiclePropulsionType = 2;
                powerType = 2;
                break;
            case 6: // CeOCD_VehPrplsnType_HEV(48V重混)
                vehiclePropulsionType = 3;
                powerType = 0;
                break;
            default:
                vehiclePropulsionType = 1;
                powerType = 1;
                break;
        }
        GMBrand = mCalibrationManager.getEnumeration(CalId.GMBrand, -1);
        GMModel = mCalibrationManager.getEnumeration(CalId.GMModel, -1);
        ENABLE_APPLICATION_NAVIGATION = mCalibrationManager.getBoolean(CalId.ENABLE_APPLICATION_NAVIGATION, true);
        P_LaneLevelNavigation_Func_Enable = mCalibrationManager.getBoolean(CalId.P_LaneLevelNavigation_Func_Enable, false);
        P_V2XMapDisplay_Func_Enable = mCalibrationManager.getBoolean(CalId.P_V2XMapDisplay_Func_Enable, false);
        P_SpeedLimitInformationSource = mCalibrationManager.getEnumeration(CalId.P_SpeedLimitInformationSource, -1);
        P_ADAS_Configuration_Infomation = mCalibrationManager.getEnumeration(CalId.P_ADAS_Configuration_Infomation, -1);
        P_RearSeatTouchPanel_Func_Enable = mCalibrationManager.getBoolean(CalId.P_RearSeatTouchPanel_Func_Enable, false);
        P_HUD_Func_Enable = mCalibrationManager.getEnumeration(CalId.P_HUD_Func_Enable, -1);
        P_Navigation_Deflection_Enable = mCalibrationManager.getBoolean(CalId.P_Navigation_Deflection_Enable, false);
        P_ARCHITECTURE = mCalibrationManager.getEnumeration(CalId.P_ARCHITECTURE, -1);
        P_Navigation_PreCondition_DataProvide_Enable = mCalibrationManager.getBoolean(CalId.P_Navigation_PreCondition_DataProvide_Enable, true);
        P_Navigaiton_Supplier = mCalibrationManager.getEnumeration(CalId.P_Navigaiton_Supplier, -1);
        P_High_Voltage_Battery_Propulsion_Total_Range_Navi = mCalibrationManager.getInteger(CalId.P_High_Voltage_Battery_Propulsion_Total_Range_Navi, -1);
        P_POISearch_Func_Enable = mCalibrationManager.getBoolean(CalId.P_POISearch_Func_Enable, true);
        P_ScenarioEngine_Func_Enable = mCalibrationManager.getBoolean(CalId.P_ScenarioEngine_Func_Enable, true);
        P_GlobalSearch_Func_Enable = mCalibrationManager.getBoolean(CalId.P_GlobalSearch_Func_Enable, true);
        P_TeamTravel_Func_Enable = mCalibrationManager.getBoolean(CalId.P_TeamTravel_Func_Enable, true);
        P_BootAnimationReplacement_Func_Enable = mCalibrationManager.getBoolean(CalId.P_BootAnimationReplacement_Func_Enable, true);
        P_IME_Func_Enable = mCalibrationManager.getBoolean(CalId.P_IME_Func_Enable, true);
        P_Wallpaper_Theme_Func_Enable = mCalibrationManager.getBoolean(CalId.P_Wallpaper_Theme_Func_Enable, true);
        P_THEME_DEFAULT_VALUE = mCalibrationManager.getEnumeration(CalId.P_THEME_DEFAULT_VALUE, -1);
        SLOPE_UP_COSTLIST = mCalibrationManager.getFloatArray("SLOPE_UP_COSTLIST");
        SLOPE_DOWN_COSTLIST = mCalibrationManager.getFloatArray("SLOPE_DOWN_COSTLIST");
        TRANS_ACCESS_COSTLIST = mCalibrationManager.getFloatArray("TRANS_ACCESS_COSTLIST");
        TRANS_DECESS_COSTLIST = mCalibrationManager.getFloatArray("TRANS_DECESS_COSTLIST");
        SPEED_COSTLIST = mCalibrationManager.getEnumFloatMap("SPEED_COSTLIST");
        AUX_COSTLIST = mCalibrationManager.getFloatArray("AUX_COSTLIST");
        VEHICLE_WEIGHT = mCalibrationManager.getInteger("VEHICLE_WEIGHT", -1);
        Logger.d(TAG, "init middle");
        mPatacServiceManager = PatacServiceManager.newInstance(AppCache.getInstance().getMContext());
        try {
            getPatacRemoteSettingsManager().registerCallback(TAG, new IPatacRemoteSettingsListener() {
                @Override
                public void onHudSettingChangedRequest(PatacRemoteSettingsManager.EHudSettingType eHudSettingType, int i) {
                    if (eHudSettingType == null) {
                        return;
                    }
                    if (eHudSettingType == PatacRemoteSettingsManager.EHudSettingType.HUD_SNOWMODE_SWITCH){
                        Logger.d(TAG, "eHudSettingType: ", eHudSettingType);
                        for (CalibrationAdapterCallback callback : mCallback.values()) {
                            callback.onHudSnowModeChanged(i == 1);
                        }
                    }
                }
            });
        } catch (PatacServiceNotConnectedException e) {
            Logger.e(TAG, "getPatacRemoteSettingsManager error: ", e.getMessage());
            e.printStackTrace();
        }
        Logger.d(TAG, "init end"); // 10ms
    }

    @Override
    public void registerCallback(final String key, final CalibrationAdapterCallback callback) {
        mCallback.put(key, callback);
    }

    @Override
    public void unregisterCallback(String key) {
        mCallback.remove(key);
    }

    @Override
    public int powerType() {
        Logger.d(TAG, "s powerType: ", powerType);
        return powerType;
    }

    @Override
    public int vehiclePropulsionType() {
        Logger.d(TAG, "s vehiclePropulsionType: ", vehiclePropulsionType);
        return vehiclePropulsionType;
    }

    @Override
    public int brand() {
        Logger.d(TAG, "s GMBrand: " + GMBrand);
        return GMBrand;
    }

    @Override
    public int model() {
        Logger.d(TAG, "s GMModel: " + GMModel);
        return GMModel;
    }

    @Override
    public boolean enableApplicationNavigation() {
        Logger.d(TAG, "s ENABLE_APPLICATION_NAVIGATION: " + ENABLE_APPLICATION_NAVIGATION);
        return ENABLE_APPLICATION_NAVIGATION;
    }

    @Override
    public boolean laneLevelNavigatioFuncEnable() {
        Logger.d(TAG, "s P_LaneLevelNavigation_Func_Enable: " + P_LaneLevelNavigation_Func_Enable);
        return P_LaneLevelNavigation_Func_Enable;
    }

    @Override
    public boolean v2xMapDisplayFuncEnable() {
        Logger.d(TAG, "s P_V2XMapDisplay_Func_Enable: " + P_V2XMapDisplay_Func_Enable);
        return P_V2XMapDisplay_Func_Enable;
    }

    @Override
    public int speedLimitInformationSource() {
        Logger.d(TAG, "s P_SpeedLimitInformationSource: " + P_SpeedLimitInformationSource);
        return P_SpeedLimitInformationSource;
    }

    @Override
    public int adasConfigurationInfomation() {
        Logger.d(TAG, "s P_ADAS_Configuration_Infomation: " + P_ADAS_Configuration_Infomation);
        return P_ADAS_Configuration_Infomation;
    }

    @Override
    public int adasConfigurationType() {
        if (ADAS_Configuration_Type == -1) {
            ADAS_Configuration_Type = mCalibrationManager.getEnumeration(CalId.ADAS_Configuration_Type, -2);
        }
        Logger.d(TAG, "s ADAS_Configuration_Type: " + ADAS_Configuration_Type);
        return ADAS_Configuration_Type;
    }

    @Override
    public boolean rearSeatTouchPanelFuncEnable() {
        Logger.d(TAG, "s P_RearSeatTouchPanel_Func_Enable: " + P_RearSeatTouchPanel_Func_Enable);
        return P_RearSeatTouchPanel_Func_Enable;
    }

    @Override
    public int hudFuncEnable() {
        Logger.d(TAG, "s P_HUD_Func_Enable: " + P_HUD_Func_Enable);
        return P_HUD_Func_Enable;
    }

    @Override
    public boolean navigationDeflectionEnable() {
        if (BuildConfig.DEBUG) Logger.d(TAG, "s P_Navigation_Deflection_Enable: " + P_Navigation_Deflection_Enable);
        return P_Navigation_Deflection_Enable;
    }

    @Override
    public int architecture() {
        Logger.d(TAG, "s P_ARCHITECTURE: " + P_ARCHITECTURE);
        return P_ARCHITECTURE;
    }

    @Override
    public boolean navigationPreConditionDataProvideEnable() {
        Logger.d(TAG, "s P_Navigation_PreCondition_DataProvide_Enable: " + P_Navigation_PreCondition_DataProvide_Enable);
        return P_Navigation_PreCondition_DataProvide_Enable;
    }

    @Override
    public int navigaitonSupplier() {
        Logger.d(TAG, "s P_Navigaiton_Supplier: " + P_Navigaiton_Supplier);
        return P_Navigaiton_Supplier;
    }

    @Override
    public int highVoltageBatteryPropulsionTotalRangeNavi() {
        Logger.d(TAG, "s P_High_Voltage_Battery_Propulsion_Total_Range_Navi: " + P_High_Voltage_Battery_Propulsion_Total_Range_Navi);
        return P_High_Voltage_Battery_Propulsion_Total_Range_Navi;
    }

    @Override
    public boolean poiSearchFuncEnable() {
        Logger.d(TAG, "s P_POISearch_Func_Enable: " + P_POISearch_Func_Enable);
        return P_POISearch_Func_Enable;
    }

    @Override
    public boolean scenarioEngineFuncEnable() {
        Logger.d(TAG, "s P_ScenarioEngine_Func_Enable: " + P_ScenarioEngine_Func_Enable);
        return P_ScenarioEngine_Func_Enable;
    }

    @Override
    public boolean globalSearchFuncEnable() {
        Logger.d(TAG, "s P_GlobalSearch_Func_Enable: " + P_GlobalSearch_Func_Enable);
        return P_GlobalSearch_Func_Enable;
    }

    @Override
    public boolean teamTravelFuncEnable() {
        Logger.d(TAG, "s P_TeamTravel_Func_Enable: " + P_TeamTravel_Func_Enable);
        return P_TeamTravel_Func_Enable;
    }

    @Override
    public boolean bootAnimationReplacementFuncEnable() {
        Logger.d(TAG, "s P_BootAnimationReplacement_Func_Enable: " + P_BootAnimationReplacement_Func_Enable);
        return P_BootAnimationReplacement_Func_Enable;
    }

    @Override
    public boolean imeFuncEnable() {
        Logger.d(TAG, "s P_IME_Func_Enable: " + P_IME_Func_Enable);
        return P_IME_Func_Enable;
    }

    @Override
    public boolean wallpaperThemeFuncEnable() {
        Logger.d(TAG, "s P_Wallpaper_Theme_Func_Enable: " + P_Wallpaper_Theme_Func_Enable);
        return P_Wallpaper_Theme_Func_Enable;
    }

    @Override
    public int themeDefaultValue() {
        Logger.d(TAG, "s P_THEME_DEFAULT_VALUE: " + P_THEME_DEFAULT_VALUE);
        return P_THEME_DEFAULT_VALUE;
    }

    @Override
    public float[] slopeUpCostlist() {
        Logger.d(TAG, "s SLOPE_UP_COSTLIST: " + Arrays.toString(SLOPE_UP_COSTLIST));
        return SLOPE_UP_COSTLIST;
    }

    @Override
    public float[] slopeDownCostlist() {
        Logger.d(TAG, "s SLOPE_DOWN_COSTLIST: " + Arrays.toString(SLOPE_DOWN_COSTLIST));
        return SLOPE_DOWN_COSTLIST;
    }

    @Override
    public float[] transAccessCostlist() {
        Logger.d(TAG, "s TRANS_ACCESS_COSTLIST: " + Arrays.toString(TRANS_ACCESS_COSTLIST));
        return TRANS_ACCESS_COSTLIST;
    }

    @Override
    public float[] transDecessCostlist() {
        Logger.d(TAG, "s TRANS_DECESS_COSTLIST: " + Arrays.toString(TRANS_DECESS_COSTLIST));
        return TRANS_DECESS_COSTLIST;
    }

    @Override
    public Map<Integer, Float> speedCostlist() {
        Logger.d(TAG, "s SPEED_COSTLIST: " + SPEED_COSTLIST);
        return SPEED_COSTLIST;
    }

    @Override
    public float[] auxCostlist() {
        Logger.d(TAG, "s AUX_COSTLIST: " + Arrays.toString(AUX_COSTLIST));
        return AUX_COSTLIST;
    }

    @Override
    public int vehicleWeight() {
        Logger.d(TAG, "s VEHICLE_WEIGHT: " + VEHICLE_WEIGHT);
        return VEHICLE_WEIGHT;
    }

    /**
     * 用于加密Vin的获取devicesId
     *
     * @return id
     */
    @Override
    public String getDeviceId() {
        try {
            return getPatacVehicleManager().getVinId();
        } catch (PatacServiceNotConnectedException e) {
            Logger.e(TAG, "getDeviceId error: ", e.getMessage());
            e.printStackTrace();
        }
        return "";
    }

    /**
     * 获取hud雪地模式开关状态
     *
     */
    @Override
    public boolean getHudSnowMode() {
        try {
            PatacRemoteSettingsManager.HudSettingInfo allHudSettingInfo = getPatacRemoteSettingsManager().getAllHudSettingInfo();
            if (allHudSettingInfo == null) {
                Logger.e(TAG, "getAllHudSettingInfo error: null");
                return false;
            }
            mSnowMode = allHudSettingInfo.getSnowMode();
            return mSnowMode;
        } catch (Exception e) {
            Logger.e(TAG, "getHudSnowMode error: ", e.getMessage());
            e.printStackTrace();
        }
        return false;
    }
    private PatacVehicleManager getPatacVehicleManager() throws PatacServiceNotConnectedException {
        if (mPatacVehicleManager == null) {
            mPatacVehicleManager = (PatacVehicleManager) mPatacServiceManager.getPatacManager(PatacServiceManager.PATAC_VEHICLE_SERVICE);
        }
        return mPatacVehicleManager;
    }

    private PatacRemoteSettingsManager getPatacRemoteSettingsManager() throws PatacServiceNotConnectedException {
        if (mPatacRemoteSettingsManager == null) {
            mPatacRemoteSettingsManager = (PatacRemoteSettingsManager) mPatacServiceManager.getPatacManager(PatacServiceManager.PATAC_REMOTE_SETTINGS_SERVICE);
        }
        return mPatacRemoteSettingsManager;
    }
}