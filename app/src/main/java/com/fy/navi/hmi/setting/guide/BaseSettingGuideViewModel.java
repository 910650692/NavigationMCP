package com.fy.navi.hmi.setting.guide;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.mapdata.MapDataFragment;
import com.fy.navi.hmi.setting.guide.platenumber.SettingPlateNumberFragment;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;
import java.util.Objects;

/**
 * @Description
 * @Author fh
 * @date 2024/12/11
 */
public class BaseSettingGuideViewModel extends BaseViewModel<SettingNaviFragment, SettingGuideModel> {
    private static final String TAG = MapDefaultFinalTag.SETTING_HMI_TAG;

    public MutableLiveData<Boolean> isAvoidLimit = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isPlateNumber = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isVehicleGuide = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isChargingPlan = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isMapModel3DUp = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isMapModel2DUp = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isMapModelNorthUp = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isRoadCondition = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isFavoritePoint = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isChargingStation = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isAutoScale = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isMapTextSizeStandard = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isMapTextSizeLarge = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isCarLogoDefault = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> isCarLogoBrand = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isCarLogoSpeed = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isEVCar = new MutableLiveData<>(false);



    public BaseSettingGuideViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingGuideModel initModel() {
        return new SettingGuideModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public void initView() {
        mModel.initView();
    }

    // 避开限行
    public Action avoidLimitClick = () -> {
        if (Boolean.FALSE.equals(isAvoidLimit.getValue()) &&
                TextUtils.isEmpty(mModel.getConfigKeyPlateNumber())) {
            isAvoidLimit.setValue(false);
            addFragment(new SettingPlateNumberFragment(), null);
        } else if (!TextUtils.isEmpty(mModel.getConfigKeyPlateNumber())) {
            boolean value = Boolean.FALSE.equals(isAvoidLimit.getValue());
            isAvoidLimit.setValue(value);
            mModel.setConfigKeyAvoidLimit(value);
        }
    };

    // 车道级导航
    public Action vehicleGuide = () -> {
        boolean value = Boolean.FALSE.equals(isVehicleGuide.getValue());
        isVehicleGuide.setValue(value);
        mModel.setGuideVehicle(value);
    };

    // 补能计划
    public Action chargingPlan = () -> {
        boolean value = Boolean.FALSE.equals(isChargingPlan.getValue());
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, value);
        isChargingPlan.setValue(value);
        mModel.setChargingPlan(value);
    };

    // 自动比例尺
    public Action autoScale = () -> {
        boolean value = Boolean.FALSE.equals(isAutoScale.getValue());
        LayerPackage.getInstance().openDynamicLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP, value);
        isAutoScale.setValue(value);
        mModel.setAutoScale(value);
    };

    public Action modifyPlateNumber = () -> {
        addFragment(new SettingPlateNumberFragment(), null);
    };

    /**
     * Dual choice controller.
     * @param key    control key
     * @param isTrue is true
     */
    public void dualChoiceControl(String key, boolean isTrue) {
        switch (key) {
            case SettingController.KEY_SETTING_IS_EV_CAR:
                isEVCar.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT:
                isAvoidLimit.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN:
                isChargingPlan.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_ROAD_CONDITION:
                isRoadCondition.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_FAVORITE_POINT:
                isFavoritePoint.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_CHARGING_STATION:
                isChargingStation.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE:
                isVehicleGuide.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_AUTO_SCALE:
                isAutoScale.setValue(isTrue);
                break;
            default:
                break;
        }
    }

    public Action openOfflineData = () -> {
        addFragment(new MapDataFragment(), null);
    };


    // 实时路况
    public Action roadConditionClick = () -> {
        boolean value = Boolean.FALSE.equals(isRoadCondition.getValue());
        MapPackage.getInstance().setTrafficStates(MapTypeId.MAIN_SCREEN_MAIN_MAP, value);
        isRoadCondition.setValue(value);
        mModel.setConfigKeyRoadEvent(value);
    };

    // 收藏点
    public Action favoritePointClick = () -> {
        boolean value = Boolean.FALSE.equals(isFavoritePoint.getValue());
        isFavoritePoint.setValue(value);
        mModel.hideOrShowFavoriteOnMainMap(value);
        mModel.setFavoritePoint(value);
    };

    // 充电站
    public Action chargingStationClick = () -> {
        boolean value = Boolean.FALSE.equals(isChargingStation.getValue());
        ArrayList<Integer> typeList = new ArrayList<>();
        typeList.add(20);
        MapPackage.getInstance().setCustomLabelTypeVisible(MapTypeId.MAIN_SCREEN_MAIN_MAP, typeList, value);
        isChargingStation.setValue(value);
        mModel.setChargingStation(value);
    };

    // 3D车头向上
    public Action switchMapModel3DUpClick = () -> {
        MapPackage.getInstance().switchMapMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, MapMode.UP_3D);
    };

    // 2D北向上
    public Action switchMapModel2DNorthClick = () -> {
        MapPackage.getInstance().switchMapMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, MapMode.NORTH_2D);
    };

    // 2D车头向上
    public Action switchMapModel2DUpClick = () -> {
        MapPackage.getInstance().switchMapMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, MapMode.UP_2D);
    };

    // 默认车标
    public Action switchMapLogoDefaultClick = () -> {
        if (mModel.getCarMode() == 1) {
            LayerPackage.getInstance().setCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_TYPE_2D);
            mModel.setCarMode(CarModeType.CAR_MODEL_TYPE_2D);
        } else {
            LayerPackage.getInstance().setCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_TYPE_3D);
            mModel.setCarMode(CarModeType.CAR_MODEL_TYPE_3D);
        }
    };

    // 品牌车标
    public Action switchMapLogoBrandClick = () -> {
        LayerPackage.getInstance().setCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_TYPE_SKELETON);
        mModel.setCarMode(CarModeType.CAR_MODEL_TYPE_SKELETON);
    };

    // 车速车标
    public Action switchMapLogoSpeedClick = () -> {
        LayerPackage.getInstance().setCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_TYPE_SPEED);
        mModel.setCarMode(CarModeType.CAR_MODEL_TYPE_SPEED);
    };

    // 标准字号
    public Action naviTextSizeStandardClick = () -> {
        MapPackage.getInstance().setMapViewTextSize(MapTypeId.MAIN_SCREEN_MAIN_MAP, 1f);
        mModel.setMapViewTextSize(true);
    };

    // 大字号
    public Action naviTextSizeLargeClick = () -> {
        MapPackage.getInstance().setMapViewTextSize(MapTypeId.MAIN_SCREEN_MAIN_MAP, 1.8f);
        mModel.setMapViewTextSize(false);
    };

    public void onPlateNumberChanged(String plateNumber) {
        boolean isPlateNumberEmpty = Objects.equals(plateNumber, "");
        if (!isPlateNumberEmpty)  {
            mView.onPlateNumberChanged(plateNumber);
            isAvoidLimit.setValue(true);
            isPlateNumber.setValue(true);
            mModel.setConfigKeyAvoidLimit(true);
        } else {
            isAvoidLimit.setValue(false);
            setPlateNumber(plateNumber);
            isPlateNumber.setValue(false);
            mModel.setConfigKeyAvoidLimit(false);
        }
    }

    public void setPlateNumber(String plateNumber) {
        boolean isPlateNumberEmpty = !Objects.equals(plateNumber, "");
        isPlateNumber.setValue(isPlateNumberEmpty);
        mView.setPlateNumber(plateNumber);
    }

    public void onMapModeChanged(boolean is3D, boolean isNorth, boolean isUp) {
        isMapModel2DUp.setValue(isUp);
        isMapModel3DUp.setValue(is3D);
        isMapModelNorthUp.setValue(isNorth);
    }

    public void onMapViewTextSizeChanged(boolean isStandard, boolean isLarge) {
        isMapTextSizeStandard.setValue(isStandard);
        isMapTextSizeLarge.setValue(isLarge);
    }

    public void onCarModeChanged(boolean isDefault, boolean isBrand, boolean isSpeed) {
        isCarLogoDefault.setValue(isDefault);
        isCarLogoBrand.setValue(isBrand);
        isCarLogoSpeed.setValue(isSpeed);
    }
}
