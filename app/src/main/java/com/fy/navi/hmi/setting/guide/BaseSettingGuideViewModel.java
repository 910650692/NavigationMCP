package com.fy.navi.hmi.setting.guide;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
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

public class BaseSettingGuideViewModel extends BaseViewModel<SettingNaviFragment, SettingGuideModel> {
    private static final String TAG = MapDefaultFinalTag.SETTING_HMI_TAG;

    public MutableLiveData<Boolean> mIsAvoidLimit = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsPlateNumber = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsVehicleGuide = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsChargingPlan = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsMapModel3DUp = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsMapModel2DUp = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsMapModelNorthUp = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsRoadCondition = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsFavoritePoint = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsChargingStation = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsAutoScale = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsMapTextSizeStandard = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsMapTextSizeLarge = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsCarLogoDefault = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mIsCarLogoBrand = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsCarLogoSpeed = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsEVCar = new MutableLiveData<>(false);



    public BaseSettingGuideViewModel(@NonNull final Application application) {
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

    /**
     * 初始化界面
     */
    public void initView() {
        mModel.initView();
    }

    // 避开限行
    public Action mAvoidLimitClick = () -> {
        if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())
                && Boolean.FALSE.equals(mIsAvoidLimit.getValue())) {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.navi_setting_offline_toast));
            mIsAvoidLimit.setValue(false);
            return;
        }
        if (Boolean.FALSE.equals(mIsAvoidLimit.getValue()) &&
                TextUtils.isEmpty(mModel.getConfigKeyPlateNumber())) {
            mIsAvoidLimit.setValue(false);
            addFragment(new SettingPlateNumberFragment(), null);
        } else if (!TextUtils.isEmpty(mModel.getConfigKeyPlateNumber())) {
            final boolean value = Boolean.FALSE.equals(mIsAvoidLimit.getValue());
            if (mModel.setConfigKeyAvoidLimit(value) == 0) {
                mIsAvoidLimit.setValue(value);
            }
        }
    };

    // 车道级导航
    public Action mVehicleGuide = () -> {
        final boolean value = Boolean.FALSE.equals(mIsVehicleGuide.getValue());
        mIsVehicleGuide.setValue(value);
        mModel.setGuideVehicle(value);
    };

    // 补能计划
    public Action mChargingPlan = () -> {
        final boolean value = Boolean.FALSE.equals(mIsChargingPlan.getValue());
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, value);
        mIsChargingPlan.setValue(value);
        mModel.setChargingPlan(value);
    };

    // 自动比例尺
    public Action mAutoScale = () -> {
        final boolean value = Boolean.FALSE.equals(mIsAutoScale.getValue());
        LayerPackage.getInstance().openDynamicLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP, value);
        mIsAutoScale.setValue(value);
        mModel.setAutoScale(value);
    };

    public Action mModifyPlateNumber = () -> {
        if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.navi_setting_offline_toast));
            return;
        }
        addFragment(new SettingPlateNumberFragment(), null);
    };

    /**
     * Dual choice controller.
     * @param key    control key
     * @param isTrue is true
     */
    public void dualChoiceControl(final String key, final boolean isTrue) {
        switch (key) {
            case SettingController.KEY_SETTING_IS_EV_CAR:
                mIsEVCar.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT:
                mIsAvoidLimit.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN:
                mIsChargingPlan.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_ROAD_CONDITION:
                mIsRoadCondition.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_FAVORITE_POINT:
                mIsFavoritePoint.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_CHARGING_STATION:
                mIsChargingStation.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE:
                mIsVehicleGuide.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_AUTO_SCALE:
                mIsAutoScale.setValue(isTrue);
                break;
            default:
                break;
        }
    }

    public Action mOpenOfflineData = () -> {
        addFragment(new MapDataFragment(), null);
    };


    // 实时路况
    public Action mRoadConditionClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsRoadCondition.getValue());
        MapPackage.getInstance().setTrafficStates(MapTypeId.MAIN_SCREEN_MAIN_MAP, value);
        mIsRoadCondition.setValue(value);
        mModel.setConfigKeyRoadEvent(value);
    };

    // 收藏点
    public Action mFavoritePointClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsFavoritePoint.getValue());
        mIsFavoritePoint.setValue(value);
        mModel.hideOrShowFavoriteOnMainMap(value);
        mModel.setFavoritePoint(value);
    };

    // 充电站
    public Action mChargingStationClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsChargingStation.getValue());
        final ArrayList<Integer> typeList = new ArrayList<>();
        typeList.add(25);
        MapPackage.getInstance().setCustomLabelTypeVisible(MapTypeId.MAIN_SCREEN_MAIN_MAP, typeList, value);
        mIsChargingStation.setValue(value);
        mModel.setChargingStation(value);
    };

    // 3D车头向上
    public Action mSwitchMapModel3DUpClick = () -> {
        MapPackage.getInstance().switchMapMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, MapMode.UP_3D);
    };

    // 2D北向上
    public Action mSwitchMapModel2DNorthClick = () -> {
        MapPackage.getInstance().switchMapMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, MapMode.NORTH_2D);
    };

    // 2D车头向上
    public Action mSwitchMapModel2DUpClick = () -> {
        MapPackage.getInstance().switchMapMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, MapMode.UP_2D);
    };

    // 默认车标
    public Action mSwitchMapLogoDefaultClick = () -> {
        if (mModel.getCarMode() == 1) {
            LayerPackage.getInstance().setCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_TYPE_2D);
            mModel.setCarMode(CarModeType.CAR_MODEL_TYPE_2D);
        } else {
            LayerPackage.getInstance().setCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_TYPE_3D);
            mModel.setCarMode(CarModeType.CAR_MODEL_TYPE_3D);
        }
    };

    // 品牌车标
    public Action mSwitchMapLogoBrandClick = () -> {
        LayerPackage.getInstance().setCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_TYPE_SKELETON);
        mModel.setCarMode(CarModeType.CAR_MODEL_TYPE_SKELETON);
    };

    // 车速车标
    public Action mSwitchMapLogoSpeedClick = () -> {
        LayerPackage.getInstance().setCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_TYPE_SPEED);
        mModel.setCarMode(CarModeType.CAR_MODEL_TYPE_SPEED);
    };

    // 标准字号
    public Action mNaviTextSizeStandardClick = () -> {
        MapPackage.getInstance().setMapViewTextSize(MapTypeId.MAIN_SCREEN_MAIN_MAP, 1f);
        mModel.setMapViewTextSize(true);
    };

    // 大字号
    public Action mNaviTextSizeLargeClick = () -> {
        MapPackage.getInstance().setMapViewTextSize(MapTypeId.MAIN_SCREEN_MAIN_MAP, 1.8f);
        mModel.setMapViewTextSize(false);
    };

    /**
     * 更新车牌号
     * @param plateNumber 车牌号
     */
    public void onPlateNumberChanged(final String plateNumber) {
        final boolean isPlateNumberEmpty = Objects.equals(plateNumber, "");
        if (!isPlateNumberEmpty)  {
            if (mModel.setConfigKeyAvoidLimit(true) == 0) {
                mIsAvoidLimit.setValue(true);
                mView.onPlateNumberChanged(plateNumber);
                mIsPlateNumber.setValue(true);
                mModel.setConfigKeyPlateNumber(plateNumber);
            }
        } else {
            if (mModel.setConfigKeyAvoidLimit(false) == 0) {
                mIsAvoidLimit.setValue(false);
                setPlateNumber(plateNumber);
                mIsPlateNumber.setValue(false);
                mModel.setConfigKeyPlateNumber("");
            }
        }
    }

    /**
     * 设置车牌号
     * @param plateNumber 车牌号
     */
    public void setPlateNumber(final String plateNumber) {
        final boolean isPlateNumberEmpty = !Objects.equals(plateNumber, "");
        mIsPlateNumber.setValue(isPlateNumberEmpty);
        mView.setPlateNumber(plateNumber);
    }

    /**
     * 视角变化
     * @param is3D  是否3D车头向上
     * @param isNorth 是否北向上
     * @param isUp 是否车头向上
     */
    public void onMapModeChanged(final boolean is3D, final boolean isNorth, final boolean isUp) {
        mIsMapModel2DUp.setValue(isUp);
        mIsMapModel3DUp.setValue(is3D);
        mIsMapModelNorthUp.setValue(isNorth);
    }

    /**
     * 地图字号变化
     * @param isStandard true 标准字号
     * @param isLarge true 大字号
     */
    public void onMapViewTextSizeChanged(final boolean isStandard, final boolean isLarge) {
        mIsMapTextSizeStandard.setValue(isStandard);
        mIsMapTextSizeLarge.setValue(isLarge);
    }

    /**
     * 车标变化
     * @param isDefault 默认车标
     * @param isBrand 品牌车标
     * @param isSpeed 车速车标
     */
    public void onCarModeChanged(final boolean isDefault, final boolean isBrand, final boolean isSpeed) {
        mIsCarLogoDefault.setValue(isDefault);
        mIsCarLogoBrand.setValue(isBrand);
        mIsCarLogoSpeed.setValue(isSpeed);
    }
}
