package com.fy.navi.hmi.setting.guide;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.hmi.mapdata.MapDataFragment;
import com.fy.navi.hmi.setting.guide.platenumber.SettingPlateNumberFragment;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
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
    public MutableLiveData<Boolean> mIsLaneLevelVisibility = new MutableLiveData<>(false);
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
    public MutableLiveData<Boolean> mIsPHEVCar = new MutableLiveData<>(false);



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

    public Action mOfflineAvoidLimitClick = () -> {
        ToastUtils.Companion.getInstance().showCustomToastView(
                ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.navi_setting_offline_toast));
    };
    // 避开限行
    public Action mAvoidLimitClick = () -> {
        if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())
                && Boolean.FALSE.equals(mIsAvoidLimit.getValue())) {
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
                sendBuryPointForSettingGuide(BuryConstant.GuideOption.AVOID_RESTRICT,
                        Boolean.TRUE.equals(mIsAvoidLimit.getValue()) ? BuryConstant.Number.SECOND : BuryConstant.Number.ONE);
            }
        }
    };

    // 车道级导航
    public Action mVehicleGuide = () -> {
        final boolean value = Boolean.FALSE.equals(mIsVehicleGuide.getValue());
        mIsVehicleGuide.setValue(value);
        mModel.setGuideVehicle(value);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.LEVEL_NAVI, value ? BuryConstant.Number.SECOND : BuryConstant.Number.ONE);
    };

    // 补能计划
    public Action mChargingPlan = () -> {
        final boolean value = Boolean.FALSE.equals(mIsChargingPlan.getValue());
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, value);
        mIsChargingPlan.setValue(value);
        mModel.setChargingPlan(value);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.CHARGING_PLAN, value ? BuryConstant.Number.SECOND : BuryConstant.Number.ONE);
    };

    // 自动比例尺
    public Action mAutoScale = () -> {
        final boolean value = Boolean.FALSE.equals(mIsAutoScale.getValue());
        mIsAutoScale.setValue(value);
        mModel.setAutoScale(value);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.AUTO_SCALE, value ? BuryConstant.Number.SECOND : BuryConstant.Number.ONE);
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
     * 设置 AvoidLimit 状态
     * @param isAvoid true 开启 false 关闭
     */
    public void setAvoidStatus(final boolean isAvoid) {
        mView.setAvoidStatus(isAvoid);
    }

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
            case SettingController.KEY_SETTING_IS_PHEV_CAR:
                mIsPHEVCar.setValue(isTrue);
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
        MapPackage.getInstance().setTrafficStates(MapType.MAIN_SCREEN_MAIN_MAP, value);
        mIsRoadCondition.setValue(value);
        sendBuryPointForSettingGuide(BuryConstant.GuideOption.MAP_CONTENT, BuryConstant.MapContent.RTTI);
    };

    // 收藏点
    public Action mFavoritePointClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsFavoritePoint.getValue());
        mIsFavoritePoint.setValue(value);
        mModel.setFavoritePoint(value);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.MAP_CONTENT, BuryConstant.MapContent.FAVORITE_POINT);
    };

    // 充电站
    public Action mChargingStationClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsChargingStation.getValue());
        final ArrayList<Integer> typeList = new ArrayList<>();
        typeList.add(25);
        MapPackage.getInstance().setCustomLabelTypeVisible(MapType.MAIN_SCREEN_MAIN_MAP, typeList, value);
        mIsChargingStation.setValue(value);
        mModel.setChargingStation(value);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.MAP_CONTENT, BuryConstant.MapContent.CHARGING_STATION);
    };

    // 3D车头向上
    public Action mSwitchMapModel3DUpClick = () -> {
        OpenApiHelper.switchMapMode(MapType.MAIN_SCREEN_MAIN_MAP, MapMode.UP_3D);
    };

    // 2D北向上
    public Action mSwitchMapModel2DNorthClick = () -> {
        OpenApiHelper.switchMapMode(MapType.MAIN_SCREEN_MAIN_MAP, MapMode.NORTH_2D);
    };

    // 2D车头向上
    public Action mSwitchMapModel2DUpClick = () -> {
        OpenApiHelper.switchMapMode(MapType.MAIN_SCREEN_MAIN_MAP, MapMode.UP_2D);
    };

    // 默认车标
    public Action mSwitchMapLogoDefaultClick = () -> {
        LayerPackage.getInstance().setCarMode(MapType.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODE_DEFAULT);
        mModel.setCarMode(CarModeType.CAR_MODE_DEFAULT);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.AUTO_SCALE, BuryConstant.CarIcon.DEFAULT);
    };

    // 品牌车标
    public Action mSwitchMapLogoBrandClick = () -> {
        LayerPackage.getInstance().setCarMode(MapType.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_BRAND);
        mModel.setCarMode(CarModeType.CAR_MODEL_BRAND);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.AUTO_SCALE, BuryConstant.CarIcon.BRAND);
    };

    // 车速车标
    public Action mSwitchMapLogoSpeedClick = () -> {
        LayerPackage.getInstance().setCarMode(MapType.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODEL_SPEED);
        mModel.setCarMode(CarModeType.CAR_MODEL_SPEED);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.AUTO_SCALE, BuryConstant.CarIcon.SPEED);
    };

    // 标准字号
    public Action mNaviTextSizeStandardClick = () -> {
        MapPackage.getInstance().setMapViewTextSize(MapType.MAIN_SCREEN_MAIN_MAP, 1.3f);
        mModel.setMapViewTextSize(true);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.MAP_WORD_SIZE, BuryConstant.MapFontSize.DEFAULT);
    };

    // 大字号
    public Action mNaviTextSizeLargeClick = () -> {
        MapPackage.getInstance().setMapViewTextSize(MapType.MAIN_SCREEN_MAIN_MAP, 1.5f);
        mModel.setMapViewTextSize(false);

        sendBuryPointForSettingGuide(BuryConstant.GuideOption.MAP_WORD_SIZE, BuryConstant.MapFontSize.BIG);
    };

    /**
     * 更新车牌号
     * @param plateNumber 车牌号
     */
    public void onPlateNumberChanged(final String plateNumber) {
        final boolean isPlateNumberEmpty = Objects.equals(plateNumber, "");
        int setConfigKeyAvoidLimitResult;
        if (!isPlateNumberEmpty) {
            mIsAvoidLimit.setValue(true);
            mView.onPlateNumberChanged(plateNumber);
            mIsPlateNumber.setValue(true);
            setConfigKeyAvoidLimitResult = mModel.setConfigKeyAvoidLimit(true);
        } else {
            mIsAvoidLimit.setValue(false);
            setPlateNumber(plateNumber);
            mIsPlateNumber.setValue(false);
            setConfigKeyAvoidLimitResult = mModel.setConfigKeyAvoidLimit(false);
        }
        Logger.d("onPlateNumberChanged: setConfigKeyAvoidLimitResult = "
                + setConfigKeyAvoidLimitResult);
    }

    /**
     * 设置车道级导航是否可见
     * @param isVisible true 显示 false 隐藏
     */
    public void setLaneLevelVisibility(final boolean isVisible) {
        mIsLaneLevelVisibility.setValue(isVisible);
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

    /**
     * 埋点
     * @param option option
     * @param value value
     */
    @HookMethod
    private void sendBuryPointForSettingGuide(final String option, final String value){
        final String eventName = switch (option){
            case BuryConstant.GuideOption.AVOID_RESTRICT ->
                BuryConstant.EventName.AMAP_SETTING_AVOIDRESTRICT;
            case BuryConstant.GuideOption.LEVEL_NAVI ->
                BuryConstant.EventName.AMAP_SETTING_LANELEVELNAVI;
            case BuryConstant.GuideOption.MAP_CONTENT ->
                BuryConstant.EventName.AMAP_SETTING_MAPCONTENT;
            case BuryConstant.GuideOption.MAP_WORD_SIZE ->
                BuryConstant.EventName.AMAP_SETTING_MAPWORDSIZE;
            case BuryConstant.GuideOption.CAR_ICON ->
                BuryConstant.EventName.AMAP_SETTING_CARICON;
            case BuryConstant.GuideOption.AUTO_SCALE ->
                BuryConstant.EventName.AMAP_SETTING_AUTOSCALE;
            case BuryConstant.GuideOption.CHARGING_PLAN ->
                BuryConstant.EventName.AMAP_SETTING_CHARGINGPLAN;
            default -> BuryConstant.EventName.AMAP_UNKNOWN;
        };
        BuryPointController.getInstance().setEventName(eventName.toString());
        final BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SETTING_CONTENT, value)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }
}
