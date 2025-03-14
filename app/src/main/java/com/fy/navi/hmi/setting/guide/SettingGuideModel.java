package com.fy.navi.hmi.setting.guide;


import com.android.utils.log.Logger;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.setting.SettingCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.base.BaseModel;


/**
 * @Description
 * @Author lvww
 * @date 2024/12/11
 */
public class SettingGuideModel extends BaseModel<SettingGuideViewModel> implements SettingCallback, SettingUpdateObservable.SettingUpdateObserver {
    private static final String TAG = SettingGuideModel.class.getName();
    private final SettingPackage settingPackage;

    public SettingGuideModel() {
        settingPackage = SettingPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        settingPackage.registerCallBack("SettingGuideModel",this);
        SettingUpdateObservable.getInstance().addObserver("SettingGuideModel", this);
    }

    /**
     * Init view.
     * 初始化各设置项状态值
     */
    public void initView() {
        getCarType();
        getAvoidLimit();
        getGuideVehicle();
        getChargingPlan();
        getRoadEvent();
        getFavoritePoint();
        getChargingStation();
        getAutoScale();
        getVehicleNumber();
        getMapModeStatus();
        getMapViewTextSize();
        updateCarMode();
    }

    public void getAvoidLimit() {
        boolean avoidLimit = settingPackage.getConfigKeyAvoidLimit();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT, avoidLimit);
    }

    public void getGuideVehicle() {
        boolean vehicleGuide = settingPackage.getGuideVehicle();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE, vehicleGuide);
    }

    public void getChargingPlan() {
        boolean chargingPlan = settingPackage.getChargingPlan();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, chargingPlan);
    }

    public void getRoadEvent() {
        boolean roadEvent = settingPackage.getConfigKeyRoadEvent();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_ROAD_CONDITION, roadEvent);
    }

    public void getFavoritePoint() {
        boolean favoritePoint = settingPackage.getFavoritePoint();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_FAVORITE_POINT, favoritePoint);
    }

    public void getChargingStation() {
        boolean serviceCenter = settingPackage.getChargingStation();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_CHARGING_STATION, serviceCenter);
    }

    public void getAutoScale() {
        boolean autoScale = settingPackage.getAutoScale();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_AUTO_SCALE, autoScale);
    }

    private void getMapModeStatus() {
        int mapMode = settingPackage.getConfigKeyMapviewMode();
        switch (mapMode) {
            case 1:
                mViewModel.onMapModeChanged(false, true, false);
                break;
            case 2:
                mViewModel.onMapModeChanged(true, false, false);
                break;
            default:
                mViewModel.onMapModeChanged(false, false, true);
                break;
        }
    }

    private void getMapViewTextSize() {
        boolean isStandard = settingPackage.getMapViewTextSize();
        mViewModel.onMapViewTextSizeChanged(isStandard, !isStandard);
    }

    private void updateCarMode() {
        int carMode = settingPackage.getCarMode();
        switch (carMode) {
            case 0:
            case 1:
                mViewModel.onCarModeChanged(true, false, false);
                break;
            case 2 :
                mViewModel.onCarModeChanged(false, true, false);
                break;
            case 3:
                mViewModel.onCarModeChanged(false, false, true);
                break;
        }
    }

    public int getCarMode() {
        return settingPackage.getCarMode();
    }

    public void setCarMode(@CarModeType.CarModelTypeId int carMode) {
        settingPackage.setCarMode(carMode);
    }

    public void setMapViewTextSize(boolean isStandard) {
        settingPackage.setMapViewTextSize(isStandard);
    }

    public void setConfigKeyAvoidLimit(boolean avoidLimit) {
        settingPackage.setConfigKeyAvoidLimit(avoidLimit);
    }

    public void setAutoScale(boolean isOpen) {
        settingPackage.setAutoScale(isOpen);
    }

    public void setGuideVehicle(boolean isGuideVehicle) {
        settingPackage.setGuideVehicle(isGuideVehicle);
    }

    public void setFavoritePoint(boolean isFavoritePoint) {
        settingPackage.setFavoritePoint(isFavoritePoint);
    }

    public void setChargingStation(boolean isChargingStation) {
        settingPackage.setChargingStation(isChargingStation);
    }

    public void setChargingPlan(boolean isChargingPlan) {
        settingPackage.setChargingPlan(isChargingPlan);
    }

    public void setConfigKeyRoadEvent(boolean roadEvent) {
        settingPackage.setConfigKeyRoadEvent(roadEvent);
    }

    public void setConfigKeyPlateNumber(String carNumber) {
        settingPackage.setConfigKeyPlateNumber(carNumber);
    }

    public String getConfigKeyPlateNumber() {
        return settingPackage.getConfigKeyPlateNumber();
    }
    // 获取当前用户车牌号
    private void getVehicleNumber() {
        mViewModel.setPlateNumber(settingPackage.getConfigKeyPlateNumber());
    }

    public void getCarType() {
        int carMode = CalibrationPackage.getInstance().powerType();
        boolean isEVCar = carMode == 1;
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_IS_EV_CAR, isEVCar);
    }

    public void hideOrShowFavoriteOnMainMap(boolean isFavoritePoint) {
        settingPackage.hideOrShowFavoriteOnMainMap(isFavoritePoint);
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        SettingUpdateObservable.getInstance().removeObserver("SettingGuideModel", this);
    }

    @Override
    public void notify(int eventType, int exCode) {
        // 数据发生变化，通知HMI更新UI
        // updateInterface(key, value);
    }

    @Override
    public void onPlateNumberChanged(String plateNumber) {
        Logger.d("plateNumberInputFinish" + plateNumber);
        setConfigKeyPlateNumber(plateNumber);
        mViewModel.onPlateNumberChanged(plateNumber);
    }
}
