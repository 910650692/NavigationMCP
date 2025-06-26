package com.sgm.navi.service.logicpaket.setting;


import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.R;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.setting.SettingAdapter;
import com.sgm.navi.service.adapter.setting.SettingAdapterCallback;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.route.RoutePreferenceID;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.greendao.favorite.FavoriteManager;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.utils.SettingsPrivacyManager;

import java.util.Hashtable;

public final class SettingPackage implements SettingAdapterCallback {

    public static final String TAG = SettingPackage.class.getSimpleName();

    private final SettingAdapter mSettingAdapter;
    private final Hashtable<String, SettingCallback> mCallbackList;
    private final SettingManager mSettingManager;
    private final Hashtable<String, SettingChangeCallback> mChangeCallbackList;
    private final LayerAdapter mLayerAdapter;
    private final FavoriteManager mFavoriteManager;
    //是否当前上电内关闭的补能开关
    private boolean mIsCurCloseChargingPlan;
    public static SettingPackage getInstance() {
        return SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final SettingPackage INSTANCE = new SettingPackage();
    }

    private SettingPackage() {
        mCallbackList = new Hashtable<>();
        mChangeCallbackList = new Hashtable<>();
        mSettingAdapter = SettingAdapter.getInstance();
        mSettingManager = new SettingManager();
        mSettingManager.init();
        mFavoriteManager = FavoriteManager.getInstance();
        mFavoriteManager.init();
        mLayerAdapter = LayerAdapter.getInstance();
    }

    /**
     * 注册回调
     *
     * @param key      回调key
     * @param callback 回调
     */
    public synchronized void registerCallBack(final String key, final SettingCallback callback) {
        if (callback != null && !mCallbackList.contains(callback)) {
            mCallbackList.put(key, callback);
        }
    }

    /**
     * 监听设置项实时变化
     *
     * @param key      回调key
     * @param callback 回调
     */
    public synchronized void setSettingChangeCallback(final String key, final SettingChangeCallback callback) {
        if (callback != null && !mChangeCallbackList.contains(callback)) {
            mChangeCallbackList.put(key, callback);
        }
    }

    /**
     * 注销回调
     * @param key 回调key
     */
    public void unRegisterCallBack(final String key) {
        mCallbackList.remove(key);
    }

    /**
     * 注销回调
     * @param key 回调key
     */
    public void unRegisterSettingChangeCallback(final String key) {
        mChangeCallbackList.remove(key);
    }


    /**
     * 设置初始化
     */
    public void init() {
        mSettingAdapter.initSetting();
        mSettingAdapter.registerCallback(TAG, this);
        initAllSetting();

        SettingsPrivacyManager.getInstance().init();
    }

    @Override
    public void notify(final int eventType, final int exCode) {
        for (SettingCallback callback : mCallbackList.values()) {
            callback.notify(eventType, exCode);
        }
    }

    @Override
    public void onSettingChanged(final String key, final String value) {
        for (SettingChangeCallback callback : mChangeCallbackList.values()) {
            callback.onSettingChanged(key, value);
        }
    }

    /**
     * 从数据库获取数据
     *
     * @param key 数据库key
     * @return 数据库value
     */
    public String getValueFromDB(final String key) {
        return mSettingManager.getValueByKey(key);
    }

    /**
     * 设置算路偏好
     *
     * @param routePreferenceID 算路偏好
     */
    public void setRoutePreference(final RoutePreferenceID routePreferenceID) {
        if (mSettingAdapter.setConfigKeyPlanPref(routePreferenceID) == 0) {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE, formatPreferenceToDB(routePreferenceID));
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE, formatPreferenceToDB(routePreferenceID));
            }
        }
    }

    /**
     * 提供给语音--设置偏好
     * @param routePreferenceID 偏好ID
     */
    public void setRoutePreferenceByVoice(final RoutePreferenceID routePreferenceID) {
        setRoutePreference(routePreferenceID);
        for (SettingChangeCallback callback : mChangeCallbackList.values()) {
            if (ConvertUtils.isEmpty(callback)) {
                continue;
            }
            callback.onRoutePreferenceChange(routePreferenceID);
        }
    }

    /**
     * 将算路偏好转换为数据库存储的算路偏好
     *
     * @param routePreference 算路偏好
     * @return 数据库对应的算路偏好
     */
    public String formatPreferenceToDB(final RoutePreferenceID routePreference) {
        return switch (routePreference) {
            case PREFERENCE_RECOMMEND -> SettingController.VALUE_ROUTE_PREFERENCE_RECOMMEND;
            case PREFERENCE_AVOIDCONGESTION ->
                    SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION;
            case PREFERENCE_LESSCHARGE -> SettingController.VALUE_ROUTE_PREFERENCE_LESS_CHARGE;
            case PREFERENCE_NOTHIGHWAY -> SettingController.VALUE_ROUTE_PREFERENCE_NOT_HIGHWAY;
            case PREFERENCE_FIRSTHIGHWAY -> SettingController.VALUE_ROUTE_PREFERENCE_FIRST_HIGHWAY;
            case PREFERENCE_FIRSTMAINROAD ->
                    SettingController.VALUE_ROUTE_PREFERENCE_FIRST_MAIN_ROAD;
            case PREFERENCE_FASTESTSPEED -> SettingController.VALUE_ROUTE_PREFERENCE_FASTEST_SPEED;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE ->
                    SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE;
            case PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY ->
                    SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_NOT_HIGHWAY;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY ->
                    SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_HIGHWAY;
            case PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY ->
                    SettingController.VALUE_ROUTE_PREFERENCE_LESS_CHARGE_AND_NOT_HIGHWAY;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY ->
                    SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE_AND_NOT_HIGHWAY;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD ->
                    SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_MAIN_ROAD;
            case PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED ->
                    SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FASTEST_SPEED;
        };
    }


    /**
     * 获取算路偏好
     *
     * @return 算路偏好
     */
    public RoutePreferenceID getRoutePreference() {
        final RoutePreferenceID routePreferenceID;
        final String data = mSettingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE);
        if (!data.isEmpty()) {
            routePreferenceID = getRoutePreferenceFromDB();
        } else {
            mSettingAdapter.setConfigKeyPlanPref(RoutePreferenceID.PREFERENCE_RECOMMEND);
            routePreferenceID = mSettingAdapter.getConfigKeyPlanPref();
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE, formatPreferenceToDB(routePreferenceID));
        }
        return routePreferenceID;
    }

    /**
     * 从数据库获取算路偏好
     *
     * @return 算路偏好
     */
    private RoutePreferenceID getRoutePreferenceFromDB() {
        final String routePreference = mSettingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE);
        return switch (routePreference) {
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION ->
                    RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
            case SettingController.VALUE_ROUTE_PREFERENCE_LESS_CHARGE ->
                    RoutePreferenceID.PREFERENCE_LESSCHARGE;
            case SettingController.VALUE_ROUTE_PREFERENCE_NOT_HIGHWAY ->
                    RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
            case SettingController.VALUE_ROUTE_PREFERENCE_FIRST_HIGHWAY ->
                    RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
            case SettingController.VALUE_ROUTE_PREFERENCE_FIRST_MAIN_ROAD ->
                    RoutePreferenceID.PREFERENCE_FIRSTMAINROAD;
            case SettingController.VALUE_ROUTE_PREFERENCE_FASTEST_SPEED ->
                    RoutePreferenceID.PREFERENCE_FASTESTSPEED;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE ->
                    RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_NOT_HIGHWAY ->
                    RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_HIGHWAY ->
                    RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY;
            case SettingController.VALUE_ROUTE_PREFERENCE_LESS_CHARGE_AND_NOT_HIGHWAY ->
                    RoutePreferenceID.PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE_AND_NOT_HIGHWAY ->
                    RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_MAIN_ROAD ->
                    RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FASTEST_SPEED ->
                    RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED;
            default -> RoutePreferenceID.PREFERENCE_RECOMMEND;
        };
    }

    /**
     * 设置避开限行状态  false 关闭 true 打开
     *
     * @param avoidLimit false 关闭 true 打开
     * @return code 0 成功 其他 失败
     */
    public int setConfigKeyAvoidLimit(final boolean avoidLimit) {
        final int code = mSettingAdapter.setConfigKeyAvoidLimit(avoidLimit);
        if (code == 0) {
            if (avoidLimit) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.avoid_limit_open_success));
            }
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT, String.valueOf(avoidLimit));
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT, String.valueOf(avoidLimit));
            }
        } else {
            if (avoidLimit) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.avoid_limit_open_failed));
            }
        }
        return code;
    }

    /**
     * 获取避开限行状态
     *
     * @return false 关闭 true 打开
     */
    public boolean getConfigKeyAvoidLimit() {
        boolean avoidLimit = false;
        final String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);

        if (!TextUtils.isEmpty(value)) {
            avoidLimit = Boolean.parseBoolean(value);
        } else {
            setConfigKeyAvoidLimit(avoidLimit);
        }
        return avoidLimit;
    }

    /**
     * 设置车牌号
     *
     * @param carNumber 车牌号
     */
    public void setConfigKeyPlateNumber(final String carNumber) {
        final int code = mSettingAdapter.setConfigKeyPlateNumber(carNumber);
        if (code == 0) {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER, carNumber);
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER, carNumber);
            }
        }
    }

    /**
     * 获取车牌号
     *
     * @return 车牌号
     */
    public String getConfigKeyPlateNumber() {
        String carNumber = "";
        final String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        if (!TextUtils.isEmpty(value)) {
            carNumber = value;
        } else {
            setConfigKeyPlateNumber(carNumber);
        }
        return carNumber;
    }

    /**
     * 打开或者关闭自动比例尺
     *
     * @param isOpen true 打开 false 关闭
     */
    public void setAutoScale(final boolean isOpen) {
        if (!isOpen) {
            mLayerAdapter.closeDynamicLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        }
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_AUTO_SCALE, String.valueOf(isOpen));
        for (SettingChangeCallback callback : mChangeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_AUTO_SCALE, String.valueOf(isOpen));
        }
    }

    /**
     * 判断是否打开自动比例尺
     *
     * @return true 打开 false 关闭
     */
    public boolean getAutoScale() {
        String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_AUTO_SCALE);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
            setAutoScale(true);
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置是否开启车道级导航
     *
     * @param isGuideVehicle true 打开 false 关闭
     */
    public void setGuideVehicle(final boolean isGuideVehicle) {
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE, String.valueOf(isGuideVehicle));
        for (SettingChangeCallback callback : mChangeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE, String.valueOf(isGuideVehicle));
        }
    }

    /**
     * 设置是否开启车道级导航
     *
     * @return true 打开 false 关闭
     */
    public boolean getGuideVehicle() {
        String value = getValueFromDB(SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
            setGuideVehicle(true);
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置是否开启补能计划
     *
     * @param isChargingPlan true 打开 false 关闭
     */
    public void setChargingPlan(final boolean isChargingPlan) {
        Logger.i(TAG, "isChargingPlan:", isChargingPlan);
        mIsCurCloseChargingPlan = !isChargingPlan;
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, String.valueOf(isChargingPlan));
        for (SettingChangeCallback callback : mChangeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, String.valueOf(isChargingPlan));
        }
    }

    /**
     * 设置是否开启补能计划
     *
     * @return true 打开 false 关闭
     */
    public boolean getChargingPlan() {
        String value = getValueFromDB(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
            setChargingPlan(true);
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 是否当前上电内关闭的补能开关
     *
     * @return
     */
    public boolean getCurCloseChargingPlan() {
        return mIsCurCloseChargingPlan;
    }

    /**
     * 设置显示收藏点
     *
     * @param isFavoritePoint true 打开 false 关闭
     */
    public void setFavoritePoint(final boolean isFavoritePoint) {
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_FAVORITE_POINT, String.valueOf(isFavoritePoint));
        for (SettingChangeCallback callback : mChangeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_FAVORITE_POINT, String.valueOf(isFavoritePoint));
        }
    }

    /**
     * 获取是否显示收藏点
     *
     * @return true 打开 false 关闭
     */
    public boolean getFavoritePoint() {
        String value = getValueFromDB(SettingController.KEY_SETTING_FAVORITE_POINT);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
            setFavoritePoint(true);
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置显示充电桩
     *
     * @param isChargingStation true 打开 false 关闭
     */
    public void setChargingStation(final boolean isChargingStation) {
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_CHARGING_STATION, String.valueOf(isChargingStation));
        for (SettingChangeCallback callback : mChangeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_CHARGING_STATION, String.valueOf(isChargingStation));
        }
    }

    /**
     * 获取是否显示充电桩
     *
     * @return true 打开 false 关闭
     */
    public boolean getChargingStation() {
        String value = getValueFromDB(SettingController.KEY_SETTING_CHARGING_STATION);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
            setChargingStation(true);
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置车标模式
     *
     * @param carMode 车标模式 0: 2D默认车标  1: 3D默认车标 2: 3D骨骼车标  3: 3D车速车标
     */
    public void setCarMode(final CarModeType carMode) {
        switch (carMode) {
            case CAR_MODEL_BRAND:
                mSettingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_BRAND);
                for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                    callback.onSettingChanged(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_BRAND);
                }
                break;
            case CAR_MODEL_SPEED:
                mSettingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_SPEED);
                for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                    callback.onSettingChanged(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_SPEED);
                }
                break;
            default:
                mSettingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_DEFAULT);
                for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                    callback.onSettingChanged(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_DEFAULT);
                }
                break;
        }

    }

    /**
     * 获取车标模式
     *
     * @return carMode  0: 2D默认车标  1: 3D默认车标 2: 3D骨骼车标  3: 3D车速车标
     */
    public CarModeType getCarMode() {
        CarModeType carMode = CarModeType.CAR_MODE_DEFAULT;
        final String data = getValueFromDB(SettingController.KEY_SETTING_CAR_LOGO);
        if (!TextUtils.isEmpty(data)) {
            switch (data) {
                case SettingController.VALUE_NAVI_CAR_LOGO_BRAND:
                    carMode = CarModeType.CAR_MODEL_BRAND;
                    break;
                case SettingController.VALUE_NAVI_CAR_LOGO_SPEED:
                    carMode = CarModeType.CAR_MODEL_SPEED;
                    break;
                default:
                    carMode = CarModeType.CAR_MODE_DEFAULT;
                    break;
            }
        } else {
            LayerPackage.getInstance().setCarMode(MapType.MAIN_SCREEN_MAIN_MAP, CarModeType.CAR_MODE_DEFAULT);
            LayerPackage.getInstance().setCarMode(MapType.CLUSTER_MAP, CarModeType.CAR_MODE_DEFAULT);
            setCarMode(CarModeType.CAR_MODE_DEFAULT);
        }
        return carMode;
    }

    /**
     * 设置地图文字大小
     *
     * @param isStandard true 标准字号 false 大号字
     */
    public void setMapViewTextSize(final boolean isStandard) {
        if (isStandard) {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_TEXT_SIZE, SettingController.VALUE_NAVI_TEXT_SIZE_STANDARD);
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_TEXT_SIZE, SettingController.VALUE_NAVI_TEXT_SIZE_STANDARD);
            }
        } else {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_TEXT_SIZE, SettingController.VALUE_NAVI_TEXT_SIZE_LARGE);
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_TEXT_SIZE, SettingController.VALUE_NAVI_TEXT_SIZE_LARGE);
            }
        }
    }

    /**
     * 获取地图文字大小
     *
     * @return true 标准字号 false 大号字
     */
    public boolean getMapViewTextSize() {
        String value = getValueFromDB(SettingController.KEY_SETTING_TEXT_SIZE);
        if (TextUtils.isEmpty(value)) {
            MapPackage.getInstance().setMapViewTextSize(MapType.MAIN_SCREEN_MAIN_MAP, 1f);
            value = SettingController.VALUE_NAVI_TEXT_SIZE_STANDARD;
            setMapViewTextSize(true);
        }
        return switch (value) {
            case SettingController.VALUE_NAVI_TEXT_SIZE_STANDARD -> true;
            case SettingController.VALUE_NAVI_TEXT_SIZE_LARGE -> false;
            default -> true;
        };
    }

    /**
     * 设置导航播报模式
     *
     * @param broadcastMode 导航播报模式 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    public void setConfigKeyBroadcastMode(final int broadcastMode) {
        final int code = mSettingAdapter.setConfigKeyBroadcastMode(broadcastMode);
        if (code == 0) {
            switch (broadcastMode) {
                case 1:
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_CONCISE);
                    }
                    break;
                case 2:
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_DETAIL);
                    }
                    break;
                case 3:
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_SIMPLE);
                    }
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * 获取导航播报模式
     *
     * @return 导航播报模式 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    public int getConfigKeyBroadcastMode() {
        int broadcastMode = 2;
        final String data = getValueFromDB(SettingController.KEY_SETTING_NAVI_BROADCAST);
        if (!TextUtils.isEmpty(data)) {
            broadcastMode = switch (data) {
                case SettingController.VALUE_NAVI_BROADCAST_CONCISE -> 1;
                case SettingController.VALUE_NAVI_BROADCAST_DETAIL -> 2;
                case SettingController.VALUE_NAVI_BROADCAST_SIMPLE -> 3;
                default -> broadcastMode;
            };
        } else {
            NaviPackage.getInstance().updateBroadcastParam(broadcastMode, true);
            setConfigKeyBroadcastMode(broadcastMode);
        }
        return broadcastMode;
    }

    /**
     * 设置巡航播报前方路况
     *
     * @param roadWarn 巡航播报前方路况 true：开启 false：关闭
     */
    public void setConfigKeyRoadWarn(final boolean roadWarn) {
        final int code = mSettingAdapter.setConfigKeyRoadWarn(roadWarn);
        if (code == 0 && getCruiseBroadcastOpen()) {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS, String.valueOf(roadWarn));
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS, String.valueOf(roadWarn));
            }
        }
    }

    /**
     * 获取巡航播报前方路况
     *
     * @return 巡航播报前方路况 true：开启 false：关闭
     */
    public boolean getConfigKeyRoadWarn() {
        final boolean roadWarn;
        final String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS);
        if (!TextUtils.isEmpty(value)) {
            roadWarn = Boolean.parseBoolean(value);
        } else {
            roadWarn = getCruiseBroadcastOpen();
            setConfigKeyRoadWarn(roadWarn);
        }
        return roadWarn;
    }

    /**
     * 设置巡航播报开关
     *
     * @param isOpen true：开启 false：关闭
     */
    public void setCruiseBroadcastOpen(final boolean isOpen) {
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_CRUISE_BROADCAST, String.valueOf(isOpen));
        for (SettingChangeCallback callback : mChangeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_CRUISE_BROADCAST, String.valueOf(isOpen));
        }
    }

    /**
     * 获取巡航播报开关.
     *
     * @return true：开启 false：关闭
     */
    public boolean getCruiseBroadcastOpen() {
        String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_CRUISE_BROADCAST);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
            setCruiseBroadcastOpen(true);
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置巡航播报电子眼播报
     *
     * @param safeBroadcast 巡航播报电子眼播报 true：开启 false：关闭
     */
    public void setConfigKeySafeBroadcast(final boolean safeBroadcast) {
        final int code = mSettingAdapter.setConfigKeySafeBroadcast(safeBroadcast);
        if (code == 0 && getCruiseBroadcastOpen()) {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE, String.valueOf(safeBroadcast));
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE, String.valueOf(safeBroadcast));
            }
        }
    }

    /**
     * 获取巡航播报电子眼播报
     *
     * @return 巡航播报电子眼播报 true：开启 false：关闭
     */
    public boolean getConfigKeySafeBroadcast() {
        final boolean safeBroadcast;
        final String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE);
        if (!TextUtils.isEmpty(value)) {
            safeBroadcast = Boolean.parseBoolean(value);
        } else {
            safeBroadcast = getCruiseBroadcastOpen();
            setConfigKeySafeBroadcast(safeBroadcast);
        }
        return safeBroadcast;
    }

    /**
     * 设置巡航播报安全提醒
     *
     * @param driveWarn 巡航播报安全提醒 true：开启 false：关闭
     */
    public void setConfigKeyDriveWarn(final boolean driveWarn) {
        final int code = mSettingAdapter.setConfigKeyDriveWarn(driveWarn);
        if (code == 0 && getCruiseBroadcastOpen()) {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER, String.valueOf(driveWarn));
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER, String.valueOf(driveWarn));
            }
        }
    }

    /**
     * 获取巡航播报安全提醒
     *
     * @return 巡航播报安全提醒 true：开启 false：关闭
     */
    public boolean getConfigKeyDriveWarn() {
        final boolean driveWarn;
        final String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER);
        if (!TextUtils.isEmpty(value)) {
            driveWarn = Boolean.parseBoolean(value);
        } else {
            driveWarn = getCruiseBroadcastOpen();
            setConfigKeyDriveWarn(driveWarn);
        }
        return driveWarn;
    }

    /**
     * 设置地图视角
     *
     * @param mapViewMode 地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     */
    public void setConfigKeyMapviewMode(final int mapViewMode) {
        final int code = mSettingAdapter.setConfigKeyMapviewMode(mapViewMode);
        if (code == 0) {
            switch (mapViewMode) {
                case 0:
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_2D);
                    }
                    break;
                case 1:
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_NORTH_2D);
                    }
                    break;
                case 2:
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_3D);
                    }
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * 获取地图视角
     *
     * @return 地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     */
    public int getConfigKeyMapviewMode() {
        int mapViewMode = 0;
        final String data = getValueFromDB(SettingController.SETTING_GUIDE_MAP_MODE);
        if (!TextUtils.isEmpty(data)) {
            switch (data) {
                case SettingController.VALUE_MAP_MODE_CAR_2D:
                    break;
                case SettingController.VALUE_MAP_MODE_NORTH_2D:
                    mapViewMode = 1;
                    break;
                case SettingController.VALUE_MAP_MODE_CAR_3D:
                    mapViewMode = 2;
                    break;
                default:
                    break;
            }
        } else {
            MapAdapter.getInstance().setMapMode(MapType.MAIN_SCREEN_MAIN_MAP, MapMode.UP_2D, true);
            setConfigKeyMapviewMode(mapViewMode);
        }
        return mapViewMode;
    }

    /**
     * 设置路况开关
     *
     * @param roadEvent 路况开关 true：开启 false：关闭
     */
    public void setConfigKeyRoadEvent(final boolean roadEvent) {
        final int code = mSettingAdapter.setConfigKeyRoadEvent(roadEvent);
        if (code == 0) {
            for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_ROAD_CONDITION, String.valueOf(roadEvent));
            }
        }
    }

    /**
     * 获取路况开关
     *
     * @return 路况开关 true：开启 false：关闭
     */
    public boolean getConfigKeyRoadEvent() {
        boolean roadEvent = true;
        final String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_ROAD_CONDITION);
        if (!TextUtils.isEmpty(value)) {
            roadEvent = Boolean.parseBoolean(value);
        } else {
            MapPackage.getInstance().setTrafficStates(MapType.MAIN_SCREEN_MAIN_MAP, true);
            setConfigKeyRoadEvent(roadEvent);
        }
        return roadEvent;
    }

    /**
     * 设置静音状态
     *
     * @param mute 静音状态 0：不静音，默认态； 1：静音
     */
    public void setConfigKeyMute(final int mute) {
        final int code = mSettingAdapter.setConfigKeyMute(mute);
        if (code == 0) {
            switch (mute) {
                case 0:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_ON);
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_ON);
                    }
                    break;
                case 1:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_OFF);
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_OFF);
                    }
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * 获取静音状态
     *
     * @return 静音状态 0：不静音，默认态； 1：静音
     */
    public int getConfigKeyMute() {
        int mute = 0;
        final String data = getValueFromDB(SettingController.KEY_SETTING_VOICE_MUTE);
        if (!TextUtils.isEmpty(data)) {
            switch (data) {
                case SettingController.VALUE_VOICE_MUTE_ON:
                    break;
                case SettingController.VALUE_VOICE_MUTE_OFF:
                    mute = 1;
                    break;
                default:
                    break;
            }
        } else {
            mute = mSettingAdapter.getConfigKeyMute();
            setConfigKeyMute(mute);
        }
        return mute;
    }

    /**
     * 设置白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     *
     * @param dayNightMode 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     */
    public void setConfigKeyDayNightMode(final ThemeType dayNightMode) {
        final int code = mSettingAdapter.setConfigKeyDayNightMode(dayNightMode);
        if (code == 0) {
            switch (ThemeType.getThemeValueByType(dayNightMode)) {
                case 16:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_AUTO);
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_AUTO);
                    }
                    break;
                case 17:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_DAYTIME);
                    for (SettingChangeCallback callback : mChangeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_DAYTIME);
                    }
                    break;
                case 18:

                    break;
                default:
                    break;
            }
        }
    }

    /**
     * 获取白天黑夜
     *
     * @return 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     */
    public int getConfigKeyDayNightMode() {
        final int dayNightMode;
        final String data = getValueFromDB(SettingController.KEY_SETTING_DISPLAY_MODE);
        if (!TextUtils.isEmpty(data)) {
            dayNightMode = switch (data) {
                case SettingController.VALUE_DISPLAY_MODE_DAYTIME -> 17;
                case SettingController.VALUE_DISPLAY_MODE_NIGHT -> 18;
                default -> 16;
            };
        } else {
            dayNightMode = mSettingAdapter.getConfigKeyDayNightMode();
            switch (dayNightMode) {
                case 16:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_AUTO);
                    break;
                case 17:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_DAYTIME);
                    break;
                case 18:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_NIGHT);
                    break;
                default:
                    break;
            }
        }
        return dayNightMode;
    }

    /**
     * 设置授权状态
     *
     * @param isOneYearPrivacy true：授权一年 false：永不授权
     *
     */
    public void setPrivacyStatus(final boolean isOneYearPrivacy) {
        SettingsPrivacyManager.getInstance().setLocationPrivacyStatus(isOneYearPrivacy);
    }

    /**
     * 获取授权状态
     *
     * @return true：授权一年 false：永不授权
     */
    public boolean getPrivacyStatus() {
        return SettingsPrivacyManager.getInstance().getLocationPrivacyStatus();
    }

    /**
     * 获取授权到期时间
     *
     * @return 时间
     */
    public String getEndDate() {
        return SettingsPrivacyManager.getInstance().getEndDate();
    }

    public void dispatchLocationPrivacyStatus(final boolean isOneYear) {
        SettingUpdateObservable.getInstance()
                .notifySettingChanged(SettingController.KEY_SETTING_PRIVACY_STATUS, isOneYear);
    }

    /**
     * 获取是否自动记录
     */
    public void getAutoRecord() {
        final String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_IS_AUTO_RECORD);
        if (value == null || TextUtils.isEmpty(value)) {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_IS_AUTO_RECORD, String.valueOf(true));
        }
    }

    /**
     * 初始化设置
     */
    public void initAllSetting() {
        getRoutePreference();
        getConfigKeyAvoidLimit();
        getConfigKeyPlateNumber();
        getGuideVehicle();
        getChargingPlan();
        getChargingStation();
        getFavoritePoint();
        getConfigKeyRoadEvent();
        getMapViewTextSize();
        getConfigKeyMapviewMode();
        getCarMode();
        getAutoScale();
        getConfigKeyBroadcastMode();
        getCruiseBroadcastOpen();
        getConfigKeySafeBroadcast();
        getConfigKeyDriveWarn();
        getConfigKeyRoadWarn();
        getAutoRecord();
    }

    public interface SettingChangeCallback {
        /**
         * @param key   设置项的key值
         * @param value 设置项对应的value值
         */
        default void onSettingChanged(String key, String value){

        }

        /**
         * @param routePreferenceID  偏好ID
         */
        default void onRoutePreferenceChange(RoutePreferenceID routePreferenceID){

        }
    }
}