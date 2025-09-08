package com.sgm.navi.hmi.setting.guide;


import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.ui.base.BaseModel;

public class SettingGuideModel extends BaseModel<SettingGuideViewModel> implements
        SettingUpdateObservable.SettingUpdateObserver, SettingPackage.SettingChangeCallback {
    private static final String TAG = SettingGuideModel.class.getName();
    private final SettingPackage mSettingPackage;
    private final CalibrationPackage mCalibrationPackage;
    private static final String MODEL_NAME = "SettingGuideModel";

    public SettingGuideModel() {
        mSettingPackage = SettingPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        SettingUpdateObservable.getInstance().addObserver(MODEL_NAME, this);
        mSettingPackage.setSettingChangeCallback(MODEL_NAME,this);
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
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
        getPushMessage();
        getVehicleNumber();
        getMapModeStatus();
        getMapViewTextSize();
        updateCarMode();
        getLaneLevelFuncEnable();
        setNetworkConnected(isNetworkConnected());
    }

    /**
     * 设置 AvoidLimit 状态
     * @param isNetworkConnected true 开启 false 关闭
     */
    public void setNetworkConnected(final boolean isNetworkConnected) {
        mViewModel.setAvoidStatus(isNetworkConnected);
    }

    /**
     * 判断网络是否连接
     * @return true 网络连接 false 网络断开
     */
    public boolean isNetworkConnected() {
        return Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
    }

    /**
     * 获取限行状态
     */
    public void getAvoidLimit() {
        final boolean avoidLimit = mSettingPackage.getConfigKeyAvoidLimit();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT, avoidLimit);
    }

    /**
     * 获取
     */
    public void getGuideVehicle() {
        final boolean vehicleGuide = mSettingPackage.getGuideVehicle();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE, vehicleGuide);
    }

    /**
     * 获取补能计划
     */
    public void getChargingPlan() {
        final boolean chargingPlan = mSettingPackage.getChargingPlan();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, chargingPlan);
    }

    /**
     * 获取路况开关
     */
    public void getRoadEvent() {
        final boolean roadEvent = mSettingPackage.getConfigKeyRoadEvent();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_ROAD_CONDITION, roadEvent);
    }

    /**
     * 获取是否显示收藏点
     */
    public void getFavoritePoint() {
        final boolean favoritePoint = mSettingPackage.getFavoritePoint();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_FAVORITE_POINT, favoritePoint);
    }

    /**
     * 获取是否显示充电桩
     */
    public void getChargingStation() {
        final boolean serviceCenter = mSettingPackage.getChargingStation();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_CHARGING_STATION, serviceCenter);
    }

    /**
     * 判断是否打开自动比例尺
     */
    public void getAutoScale() {
        final boolean autoScale = mSettingPackage.getAutoScale();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_AUTO_SCALE, autoScale);
    }

    /**
     * 判断是否打开导航中信息推送
     */
    public void getPushMessage() {
        final boolean pushMessage = mSettingPackage.getPushMessage();
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_PUSH_MESSAGE, pushMessage);
    }

    /**
     * 获取地图视角
     */
    private void getMapModeStatus() {
        final int mapMode = mSettingPackage.getConfigKeyMapviewMode();
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

    /**
     * 获取地图文字大小
     */
    private void getMapViewTextSize() {
        final boolean isStandard = mSettingPackage.getMapViewTextSize();
        mViewModel.onMapViewTextSizeChanged(isStandard, !isStandard);
    }

    /**
     * 获取车标模式
     */
    private void updateCarMode() {
        final CarModeType carMode = mSettingPackage.getCarMode();
        switch (carMode) {
            case CAR_MODEL_BRAND:
                mViewModel.onCarModeChanged(false, true, false);
                break;
            case CAR_MODEL_SPEED:
                mViewModel.onCarModeChanged(false, false, true);
                break;
            default:
                mViewModel.onCarModeChanged(true, false, false);
                break;
        }
    }

    /**
     * 获取车标模式
     * @return carMode  0: 2D默认车标  1: 3D默认车标 2: 3D骨骼车标  3: 3D车速车标
     */
    public CarModeType getCarMode() {
        return mSettingPackage.getCarMode();
    }

    /**
     * 设置车标模式
     * @param carMode  0: 2D默认车标  1: 3D默认车标 2: 3D骨骼车标  3: 3D车速车标
     */
    public void setCarMode(final CarModeType carMode) {
        mSettingPackage.setCarMode(carMode);
    }

    /**
     * 设置地图文字大小
     * @param isStandard true 标准字号 false 大号字
     */
    public void setMapViewTextSize(final boolean isStandard) {
        mSettingPackage.setMapViewTextSize(isStandard);
    }

    /**
     * 设置限行状态
     * @param avoidLimit true 限行 false 不限行
     * @return code 0 成功 其他 失败
     */
    public int setConfigKeyAvoidLimit(final boolean avoidLimit) {
        return mSettingPackage.setConfigKeyAvoidLimit(avoidLimit);
    }

    /**
     * 设置自动比例尺
     * @param isOpen true 打开 false 关闭
     */
    public void setAutoScale(final boolean isOpen) {
        mSettingPackage.setAutoScale(isOpen);
    }

    /**
     * 设置导航中信息推送
     * @param isOpen true 打开 false 关闭
     */
    public void setPushMessage(final boolean isOpen) {
        mSettingPackage.setPushMessage(isOpen);
    }

    /**
     * 设置是否开启车道级导航
     * @param isGuideVehicle true 打开 false 关闭
     */
    public void setGuideVehicle(final boolean isGuideVehicle) {
        mSettingPackage.setGuideVehicle(isGuideVehicle);
    }

    /**
     * 设置显示收藏点
     * @param isFavoritePoint true 打开 false 关闭
     */
    public void setFavoritePoint(final boolean isFavoritePoint) {
        mSettingPackage.setFavoritePoint(isFavoritePoint);
    }

    /**
     * 设置显示充电桩
     * @param isChargingStation true 打开 false 关闭
     */
    public void setChargingStation(final boolean isChargingStation) {
        mSettingPackage.setChargingStation(isChargingStation);
    }

    /**
     * 设置是否开启补能计划
     * @param isChargingPlan true 打开 false 关闭
     */
    public void setChargingPlan(final boolean isChargingPlan) {
        mSettingPackage.setChargingPlan(isChargingPlan);
    }

    /**
     * 设置车牌号
     * @param carNumber 车牌号
     */
    public void setConfigKeyPlateNumber(final String carNumber) {
        mSettingPackage.setConfigKeyPlateNumber(carNumber);
    }

    /**
     * 获取车牌号
     * @return 车牌号
     */
    public String getConfigKeyPlateNumber() {
        return mSettingPackage.getConfigKeyPlateNumber();
    }

    /**
     * 获取车牌号
     */
    private void getVehicleNumber() {
        mViewModel.setPlateNumber(mSettingPackage.getConfigKeyPlateNumber());
    }


    /**
     * 获取车辆类型
     */
    public void getCarType() {
        final int carMode = mCalibrationPackage.powerType();
        final boolean isGasCar = carMode == 0;
        final boolean isEVCar = carMode == 1;
        final boolean isPHEVCar = carMode == 2;
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_IS_GAS_CAR, isGasCar);
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_IS_EV_CAR, isEVCar);
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_IS_PHEV_CAR, isPHEVCar);
    }

    /**
     * 获取车道级导航是否支持
     */
    private void getLaneLevelFuncEnable() {
        final boolean laneLevelFuncEnable = mCalibrationPackage.laneLevelNavigatioFuncEnable();
        // TODO: 2025/6/10 当前车型标定值不对，现在没有车型能支持车道级导航，临时改成flase
        //mViewModel.setLaneLevelVisibility(laneLevelFuncEnable);
        mViewModel.setLaneLevelVisibility(false);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        SettingUpdateObservable.getInstance().removeObserver(MODEL_NAME, this);
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(mNetworkObserver);
        mSettingPackage.unRegisterSettingChangeCallback(MODEL_NAME);
    }

    @Override
    public void onPlateNumberChanged(final String plateNumber) {
        Logger.d("plateNumberInputFinish" + plateNumber);
        mViewModel.onPlateNumberChanged(plateNumber);
    }

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetValidated() {

        }

        @Override
        public void onNetConnectSuccess() {
            setNetworkConnected(true);
        }

        @Override
        public void onNetDisConnect() {
            setNetworkConnected(false);
        }

        @Override
        public void onNetUnavailable() {

        }

        @Override
        public void onNetBlockedStatusChanged() {

        }

        @Override
        public void onNetLosing() {

        }

        @Override
        public void onNetLinkPropertiesChanged() {

        }
    };

    @Override
    public void onSettingChanged(final String key, final String value) {
        switch (key) {
            case SettingController.SETTING_GUIDE_MAP_MODE :
                switch (value) {
                    case SettingController.VALUE_MAP_MODE_CAR_2D:
                        ThreadManager.getInstance().postUi(() -> mViewModel.onMapModeChanged(false, false, true));
                        break;
                    case SettingController.VALUE_MAP_MODE_NORTH_2D:
                        ThreadManager.getInstance().postUi(() -> mViewModel.onMapModeChanged(false, true, false));
                        break;
                    case SettingController.VALUE_MAP_MODE_CAR_3D:
                        ThreadManager.getInstance().postUi(() -> mViewModel.onMapModeChanged(true, false, false));
                        break;
                    default:
                        break;
                }
                break;
            case SettingController.KEY_SETTING_ROAD_CONDITION:
                ThreadManager.getInstance().postUi(() ->
                        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_ROAD_CONDITION, Boolean.parseBoolean(value)));
                break;
            case SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE_BY_PHONE:
                ThreadManager.getInstance().postUi(() -> mViewModel.notifyRoutePreference());
                break;
            default:
                break;
        }

    }
}
