package com.fy.navi.service.logicpaket.setting;


import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.fy.navi.service.adapter.setting.SettingAdapter;
import com.fy.navi.service.adapter.setting.SettingAdapterCallback;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.setting.SimpleFavoriteItemBean;
import com.fy.navi.service.greendao.favorite.Favorite;
import com.fy.navi.service.greendao.favorite.FavoriteManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.layer.LayerPackage;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

public class SettingPackage implements SettingAdapterCallback {
    public static final String TAG = SettingPackage.class.getSimpleName();

    private final SettingAdapter settingAdapter;
    private final Hashtable<String, SettingCallback> callbackList;
    private final SettingManager settingManager;
    private final Hashtable<String, SettingChangeCallback> changeCallbackList;
    private final LayerPackage layerPackage;

    public static SettingPackage getInstance() {
        return SInstanceHolder.sInstance;
    }

    private static final class SInstanceHolder {
        static final SettingPackage sInstance = new SettingPackage();
    }

    private SettingPackage() {
        callbackList = new Hashtable<>();
        changeCallbackList = new Hashtable<>();
        settingAdapter = SettingAdapter.getInstance();
        settingManager = new SettingManager();
        settingManager.init();
        layerPackage = LayerPackage.getInstance();
    }

    public synchronized void registerCallBack(String key, SettingCallback callback) {
        if (callback != null && !callbackList.contains(callback)) {
            callbackList.put(key, callback);
        }
    }

    /**
     * 监听设置项实时变化
     */
    public synchronized void setSettingChangeCallback(String key, SettingChangeCallback callback) {
        if (callback != null && !changeCallbackList.contains(callback)) {
            changeCallbackList.put(key, callback);
        }
    }


    public void init() {
        settingAdapter.initSetting();
        settingAdapter.registerCallback("SettingPackage", this);
    }

    @Override
    public void notify(int eventType, int exCode) {
        for (SettingCallback callback : callbackList.values()) {
            callback.notify(eventType, exCode);
        }
    }


    /**
     * 从数据库获取数据
     */
    public String getValueFromDB(String key) {
        return settingManager.getValueByKey(key);
    }

    /**
     * 设置数据到数据库
     */
    public void setValueByKey(String key, String value) {
        settingManager.insertOrReplace(key, value);
    }


    /**
     * 设置算路偏好
     */
    public void setRoutePreference(RoutePreferenceID routePreferenceID) {
        if (settingAdapter.setConfigKeyPlanPref(routePreferenceID) == 0) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE, formatPreferenceToDB(routePreferenceID));
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE, formatPreferenceToDB(routePreferenceID));
            }
        }
    }

    /**
     * 将算路偏好转换为数据库存储的算路偏好
     * @param routePreference 算路偏好
     * @return 数据库对应的算路偏好
     */
    public String formatPreferenceToDB(RoutePreferenceID routePreference) {
        return switch (routePreference) {
            case PREFERENCE_RECOMMEND -> SettingController.VALUE_ROUTE_PREFERENCE_RECOMMEND;
            case PREFERENCE_AVOIDCONGESTION -> SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION;
            case PREFERENCE_LESSCHARGE -> SettingController.VALUE_ROUTE_PREFERENCE_LESS_CHARGE;
            case PREFERENCE_NOTHIGHWAY -> SettingController.VALUE_ROUTE_PREFERENCE_NOT_HIGHWAY;
            case PREFERENCE_FIRSTHIGHWAY -> SettingController.VALUE_ROUTE_PREFERENCE_FIRST_HIGHWAY;
            case PREFERENCE_FIRSTMAINROAD -> SettingController.VALUE_ROUTE_PREFERENCE_FIRST_MAIN_ROAD;
            case PREFERENCE_FASTESTSPEED -> SettingController.VALUE_ROUTE_PREFERENCE_FASTEST_SPEED;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE -> SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE;
            case PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY -> SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_NOT_HIGHWAY;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY -> SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_HIGHWAY;
            case PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY -> SettingController.VALUE_ROUTE_PREFERENCE_LESS_CHARGE_AND_NOT_HIGHWAY;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY -> SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE_AND_NOT_HIGHWAY;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD -> SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_MAIN_ROAD;
            case PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED -> SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FASTEST_SPEED;
        };
    }


    /**
     * 获取算路偏好
     * @return 算路偏好
     */
    public RoutePreferenceID getRoutePreference() {
        RoutePreferenceID routePreferenceID;
        String data  = settingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE);
        if (!data.isEmpty()) {
            routePreferenceID = getRoutePreferenceFromDB();
        } else {
            routePreferenceID = settingAdapter.getConfigKeyPlanPref();
            settingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE, formatPreferenceToDB(routePreferenceID));
        }
        return routePreferenceID;
    }

    /**
     * 从数据库获取算路偏好
     * @return 算路偏好
     */
    private RoutePreferenceID getRoutePreferenceFromDB() {
        String routePreference = settingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE);
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
     */
    public void setConfigKeyAvoidLimit(boolean avoidLimit) {
        int code = settingAdapter.setConfigKeyAvoidLimit(avoidLimit);
        if (code == 0) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT, String.valueOf(avoidLimit));
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT, String.valueOf(avoidLimit));
            }
        }
    }

    /**
     * 获取避开限行状态
     * @return false 关闭 true 打开
     */
    public boolean getConfigKeyAvoidLimit() {
        boolean avoidLimit;
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);

        if (!TextUtils.isEmpty(value)) {
            avoidLimit = Boolean.parseBoolean(value);
        } else {
            avoidLimit = settingAdapter.getConfigKeyAvoidLimit();
            settingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT, String.valueOf(avoidLimit));
        }
        return avoidLimit;
    }

    /**
     * 设置车牌号
     */
    public void setConfigKeyPlateNumber(String carNumber) {
        int code = settingAdapter.setConfigKeyPlateNumber(carNumber);
        if (code == 0) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER, carNumber);
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER,carNumber);
            }
        }
    }

    /**
     * 获取车牌号
     * @return 车牌号
     */
    public String getConfigKeyPlateNumber() {
        String carNumber;
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        if (!TextUtils.isEmpty(value)) {
            carNumber = value;
        } else {
            carNumber = settingAdapter.getConfigKeyPlateNumber();
            settingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER, carNumber);
        }
        return carNumber;
    }

    /**
     * 打开或者关闭自动比例尺
     * @param isOpen true 打开 false 关闭
     */
    public void setAutoScale(boolean isOpen) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_AUTO_SCALE, String.valueOf(isOpen));
        for (SettingChangeCallback callback : changeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_AUTO_SCALE, String.valueOf(isOpen));
        }
    }

    /**
     * 判断是否打开自动比例尺
     * @return true 打开 false 关闭
     */
    public boolean getAutoScale() {
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_AUTO_SCALE);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置是否开启车道级导航
     * @param isGuideVehicle true 打开 false 关闭
     */
    public void setGuideVehicle(boolean isGuideVehicle) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE, String.valueOf(isGuideVehicle));
        for (SettingChangeCallback callback : changeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE, String.valueOf(isGuideVehicle));
        }
    }

    /**
     * 设置是否开启车道级导航
     * @reture  true 打开 false 关闭
     */
    public boolean getGuideVehicle() {
        String value = getValueFromDB(SettingController.KEY_SETTING_GUIDE_VEHICLE_GUIDE);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_FALSE;
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置是否开启补能计划
     * @param isChargingPlan true 打开 false 关闭
     */
    public void setChargingPlan(boolean isChargingPlan) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, String.valueOf(isChargingPlan));
        for (SettingChangeCallback callback : changeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN, String.valueOf(isChargingPlan));
        }
    }

    /**
     * 设置是否开启补能计划
     * @reture  true 打开 false 关闭
     */
    public boolean getChargingPlan() {
        String value = getValueFromDB(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_FALSE;
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置显示收藏点
     * @param isFavoritePoint true 打开 false 关闭
     */
    public void setFavoritePoint(boolean isFavoritePoint) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_FAVORITE_POINT, String.valueOf(isFavoritePoint));
        for (SettingChangeCallback callback : changeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_FAVORITE_POINT, String.valueOf(isFavoritePoint));
        }
    }

    public void hideOrShowFavoriteOnMainMap(boolean isFavoritePoint) {
        if (isFavoritePoint) {
            List<Favorite> tmpList = FavoriteManager.getInstance().getFavoriteNotTop();
            Logger.i(TAG,"hideOrShowFavoriteOnMainMap:" + tmpList.size());
            ArrayList<GmBizUserFavoritePoint> list = new ArrayList<>();
            tmpList.forEach((favorite -> {
                GmBizUserFavoritePoint point = new GmBizUserFavoritePoint();
                point.favoriteType = favorite.commonName;
                point.lon = favorite.point_x;
                point.lat = favorite.point_y;
                list.add(point);
            }));
            layerPackage.updateFavoriteMain(MapTypeId.MAIN_SCREEN_MAIN_MAP, list);
        } else {
            layerPackage.clearFavoriteMain(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        }
    }

    public void hideOrShowFavoriteOnMainMap() {
        String isFavoritePointStr = settingManager.getValueByKey(SettingController.KEY_SETTING_FAVORITE_POINT);
        boolean isFavoritePoint = TextUtils.equals(isFavoritePointStr, "true");
        hideOrShowFavoriteOnMainMap(isFavoritePoint);
    }

    /**
     * 获取是否显示收藏点
     *
     * @return true 打开 false 关闭
     */
    public boolean getFavoritePoint() {
        String value = getValueFromDB(SettingController.KEY_SETTING_FAVORITE_POINT);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_FALSE;
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置显示充电桩
     * @param isChargingStation true 打开 false 关闭
     */
    public void setChargingStation(boolean isChargingStation) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_CHARGING_STATION, String.valueOf(isChargingStation));
        for (SettingChangeCallback callback : changeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_CHARGING_STATION, String.valueOf(isChargingStation));
        }
    }

    /**
     * 获取是否显示充电桩
     * @return true 打开 false 关闭
     */
    public boolean getChargingStation() {
        String value = getValueFromDB(SettingController.KEY_SETTING_CHARGING_STATION);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_FALSE;
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置车标模式
     * @param carMode 车标模式 0: 2D默认车标  1: 3D默认车标 2: 3D骨骼车标  3: 3D车速车标
     */
    public void setCarMode(int carMode) {
        switch (carMode) {
            case CarModeType.CAR_MODEL_TYPE_2D :
            case CarModeType.CAR_MODEL_TYPE_3D :
                settingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_DEFAULT);
                for (SettingChangeCallback callback : changeCallbackList.values()) {
                    callback.onSettingChanged(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_DEFAULT);
                }
                break;
            case CarModeType.CAR_MODEL_TYPE_SKELETON :
                settingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_BRAND);
                for (SettingChangeCallback callback : changeCallbackList.values()) {
                    callback.onSettingChanged(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_BRAND);
                }
                break;
            case CarModeType.CAR_MODEL_TYPE_SPEED :
                settingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_SPEED);
                for (SettingChangeCallback callback : changeCallbackList.values()) {
                    callback.onSettingChanged(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_SPEED);
                }
                break;
        }

    }

    /**
     * 获取车标模式
     * @return carMode  0: 2D默认车标  1: 3D默认车标 2: 3D骨骼车标  3: 3D车速车标
     */
    public int getCarMode() {
        int carMode = 0;
        String data = getValueFromDB(SettingController.KEY_SETTING_CAR_LOGO);
        if (!TextUtils.isEmpty(data)) {
            switch (data) {
                case SettingController.VALUE_NAVI_CAR_LOGO_DEFAULT :
                    break;
                case SettingController.VALUE_NAVI_CAR_LOGO_BRAND :
                    carMode = CarModeType.CAR_MODEL_TYPE_SKELETON;
                    break;
                case SettingController.VALUE_NAVI_CAR_LOGO_SPEED :
                    carMode = CarModeType.CAR_MODEL_TYPE_SPEED;
                    break;
            }
        } else {
            carMode = layerPackage.getCarMode(MapTypeId.MAIN_SCREEN_MAIN_MAP);
            switch (carMode) {
                case CarModeType.CAR_MODEL_TYPE_2D :
                case CarModeType.CAR_MODEL_TYPE_3D :
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_DEFAULT);
                    break;
                case CarModeType.CAR_MODEL_TYPE_SKELETON :
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_BRAND);
                    break;
                case CarModeType.CAR_MODEL_TYPE_SPEED :
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_CAR_LOGO, SettingController.VALUE_NAVI_CAR_LOGO_SPEED);
                    break;
            }
        }
        return carMode;
    }

    /**
     * 设置地图文字大小
     * @param isStandard true 标准字号 false 大号字
     */
    public void setMapViewTextSize(boolean isStandard) {
        if (isStandard) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_TEXT_SIZE, SettingController.VALUE_NAVI_TEXT_SIZE_STANDARD);
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_TEXT_SIZE, SettingController.VALUE_NAVI_TEXT_SIZE_STANDARD);
            }
        } else {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_TEXT_SIZE, SettingController.VALUE_NAVI_TEXT_SIZE_LARGE);
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_TEXT_SIZE, SettingController.VALUE_NAVI_TEXT_SIZE_LARGE);
            }
        }
    }

    /**
     * 获取地图文字大小
     * @return true 标准字号 false 大号字
     */
    public boolean getMapViewTextSize() {
        String value = getValueFromDB(SettingController.KEY_SETTING_TEXT_SIZE);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_NAVI_TEXT_SIZE_STANDARD;
        }
        return switch (value) {
            case SettingController.VALUE_NAVI_TEXT_SIZE_STANDARD -> true;
            case SettingController.VALUE_NAVI_TEXT_SIZE_LARGE -> false;
            default -> true;
        };
    }

    /**
     * 设置导航播报模式
     * @param broadcastMode 导航播报模式 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    public void setConfigKeyBroadcastMode(int broadcastMode) {
        int code = settingAdapter.setConfigKeyBroadcastMode(broadcastMode);
        if (code == 0) {
            switch (broadcastMode) {
                case 1 :
                        settingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_CONCISE);
                        for (SettingChangeCallback callback : changeCallbackList.values()) {
                            callback.onSettingChanged(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_CONCISE);
                        }
                        break;
                case 2 :
                        settingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_DETAIL);
                        for (SettingChangeCallback callback : changeCallbackList.values()) {
                            callback.onSettingChanged(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_DETAIL);
                        }
                        break;
                case 3 :
                        settingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_SIMPLE);
                        for (SettingChangeCallback callback : changeCallbackList.values()) {
                            callback.onSettingChanged(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_SIMPLE);
                        }
                        break;
            }
        }
    }

    /**
     * 获取导航播报模式
     * @return 导航播报模式 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    public int getConfigKeyBroadcastMode() {
        int broadcastMode = 2;
        String data = getValueFromDB(SettingController.KEY_SETTING_NAVI_BROADCAST);
        if (!TextUtils.isEmpty(data)) {
            broadcastMode = switch (data) {
                case SettingController.VALUE_NAVI_BROADCAST_CONCISE -> 1;
                case SettingController.VALUE_NAVI_BROADCAST_DETAIL -> 2;
                case SettingController.VALUE_NAVI_BROADCAST_SIMPLE -> 3;
                default -> broadcastMode;
            };
        } else {
            broadcastMode = settingAdapter.getConfigKeyBroadcastMode();
            switch (broadcastMode) {
                case 1 :
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_CONCISE);
                    break;
                case 2 :
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_DETAIL);
                    break;
                case 3 :
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_SIMPLE);
                    break;
            }
        }
        return broadcastMode;
    }

    /**
     * 设置巡航播报前方路况
     * @param roadWarn 巡航播报前方路况 true：开启 false：关闭
     */
    public void setConfigKeyRoadWarn(boolean roadWarn) {
        int code = settingAdapter.setConfigKeyRoadWarn(roadWarn);
        if (code == 0 && getCruiseBroadcastOpen()) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS, String.valueOf(roadWarn));
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS, String.valueOf(roadWarn));
            }
        }
    }

    /**
     *  获取巡航播报前方路况
     * @return 巡航播报前方路况 true：开启 false：关闭
     */
    public boolean getConfigKeyRoadWarn() {
        boolean roadWarn;
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS);
        if (!TextUtils.isEmpty(value)) {
            roadWarn = Boolean.parseBoolean(value);
        } else {
            roadWarn = settingAdapter.getConfigKeyRoadWarn();
            settingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS, String.valueOf(roadWarn));
        }
        return roadWarn;
    }

    /**
     * 设置巡航播报开关
     * @param isOpen true：开启 false：关闭
     */
    public void setCruiseBroadcastOpen(boolean isOpen) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_CRUISE_BROADCAST, String.valueOf(isOpen));
        for (SettingChangeCallback callback : changeCallbackList.values()) {
            callback.onSettingChanged(SettingController.KEY_SETTING_CRUISE_BROADCAST, String.valueOf(isOpen));
        }
    }

    /**
     * 获取巡航播报开关
     * @return true：开启 false：关闭
     */
    public boolean getCruiseBroadcastOpen() {
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_CRUISE_BROADCAST);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
        }
        return Boolean.parseBoolean(value);
    }

    /**
     * 设置巡航播报电子眼播报
     * @param safeBroadcast 巡航播报电子眼播报 true：开启 false：关闭
     */
    public void setConfigKeySafeBroadcast(boolean safeBroadcast) {
        int code = settingAdapter.setConfigKeySafeBroadcast(safeBroadcast);
        if (code == 0 && getCruiseBroadcastOpen()) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE, String.valueOf(safeBroadcast));
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE, String.valueOf(safeBroadcast));
            }
        }
    }

    /**
     * 获取巡航播报电子眼播报
     * @return 巡航播报电子眼播报 true：开启 false：关闭
     */
    public boolean getConfigKeySafeBroadcast() {
        boolean safeBroadcast;
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE);
        if (!TextUtils.isEmpty(value)) {
            safeBroadcast = Boolean.parseBoolean(value);
        } else {
            safeBroadcast = settingAdapter.getConfigKeySafeBroadcast();
            settingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE, String.valueOf(safeBroadcast));
        }
        return safeBroadcast;
    }

    /**
     * 设置巡航播报安全提醒
     * @param driveWarn 巡航播报安全提醒 true：开启 false：关闭
     */
    public void setConfigKeyDriveWarn(boolean driveWarn) {
        int code = settingAdapter.setConfigKeyDriveWarn(driveWarn);
        if (code == 0 && getCruiseBroadcastOpen()) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER, String.valueOf(driveWarn));
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER, String.valueOf(driveWarn));
            }
        }
    }

    /**
     * 获取巡航播报安全提醒
     * @return 巡航播报安全提醒 true：开启 false：关闭
     */
    public boolean getConfigKeyDriveWarn() {
        boolean driveWarn;
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER);
        if (!TextUtils.isEmpty(value)) {
            driveWarn = Boolean.parseBoolean(value);
        } else {
            driveWarn = settingAdapter.getConfigKeyDriveWarn();
            settingManager.insertOrReplace(SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER, String.valueOf(driveWarn));
        }
        return driveWarn;
    }

    /**
     * 设置地图视角
     * @param mapViewMode 地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     */
    public void setConfigKeyMapviewMode(int mapViewMode) {
        int code = settingAdapter.setConfigKeyMapviewMode(mapViewMode);
        if (code == 0) {
            switch (mapViewMode) {
                case 0:
                    settingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_2D);
                    for (SettingChangeCallback callback : changeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_2D);
                    }
                    break;
                case 1:
                    settingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_NORTH_2D);
                    for (SettingChangeCallback callback : changeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_NORTH_2D);
                    }
                    break;
                case 2:
                    settingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_3D);
                    for (SettingChangeCallback callback : changeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_3D);
                    }
                    break;
            }
        }
    }

    /**
     * 获取地图视角
     * @return 地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     */
    public int getConfigKeyMapviewMode() {
        int mapViewMode = 0;
        String data = getValueFromDB(SettingController.SETTING_GUIDE_MAP_MODE);
        if (!TextUtils.isEmpty(data)) {
            switch (data) {
                case SettingController.VALUE_MAP_MODE_CAR_2D:
                    mapViewMode = 0;
                    break;
                case SettingController.VALUE_MAP_MODE_NORTH_2D:
                    mapViewMode = 1;
                    break;
                case SettingController.VALUE_MAP_MODE_CAR_3D:
                    mapViewMode = 2;
                    break;
            }
        } else {
            mapViewMode = settingAdapter.getConfigKeyMapviewMode();
            switch (mapViewMode) {
                case 0:
                    settingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_2D);
                    break;
                case 1:
                    settingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_NORTH_2D);
                    break;
                case 2:
                    settingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_3D);
                    break;
            }
        }
        return mapViewMode;
    }

    /**
     * 设置路况开关
     *
     * @param roadEvent 路况开关 true：开启 false：关闭
     */
    public void setConfigKeyRoadEvent(boolean roadEvent) {
        int code = settingAdapter.setConfigKeyRoadEvent(roadEvent);
        if (code == 0) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_ROAD_CONDITION, String.valueOf(roadEvent));
            for (SettingChangeCallback callback : changeCallbackList.values()) {
                callback.onSettingChanged(SettingController.KEY_SETTING_ROAD_CONDITION, String.valueOf(roadEvent));
            }
        }
    }

    /**
     * 获取路况开关
     * @return 路况开关 true：开启 false：关闭
     */
    public boolean getConfigKeyRoadEvent() {
        boolean roadEvent;
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_ROAD_CONDITION);
        if (!TextUtils.isEmpty(value)) {
            roadEvent = Boolean.parseBoolean(value);
        } else {
            roadEvent = settingAdapter.getConfigKeyRoadEvent();
            settingManager.insertOrReplace(SettingController.KEY_SETTING_ROAD_CONDITION, String.valueOf(roadEvent));
        }
        return roadEvent;
    }

    /**
     * 设置静音状态
     * @param mute 静音状态 0：不静音，默认态； 1：静音
     */
    public void setConfigKeyMute(int mute) {
        int code = settingAdapter.setConfigKeyMute(mute);
        if (code == 0) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_VOICE_MUTE, String.valueOf(mute));
            switch (mute) {
                case 0:
                    for (SettingChangeCallback callback : changeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_OFF);
                    }
                    break;
                case 1:
                    for (SettingChangeCallback callback : changeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_ON);
                    }
                   break;
            }
        }
    }

    /**
     * 获取静音状态
     * @return 静音状态 0：不静音，默认态； 1：静音
     */
    public int getConfigKeyMute() {
        int mute = 0;
        String data = getValueFromDB(SettingController.KEY_SETTING_VOICE_MUTE);
        if (!TextUtils.isEmpty(data)) {
            switch (data) {
                case SettingController.VALUE_VOICE_MUTE_OFF:
                    break;
                case SettingController.VALUE_VOICE_MUTE_ON:
                    mute = 1;
                    break;
            }
        } else {
            mute = settingAdapter.getConfigKeyMute();
            settingManager.insertOrReplace(SettingController.KEY_SETTING_VOICE_MUTE, String.valueOf(mute));
        }
        return mute;
    }

    /**
     * 设置白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     */
    public void setConfigKeyDayNightMode(int dayNightMode) {
        int code = settingAdapter.setConfigKeyDayNightMode(dayNightMode);
        if (code == 0) {
            switch (dayNightMode) {
                case 16:
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_AUTO);
                    for (SettingChangeCallback callback : changeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_AUTO);
                    }
                    break;
                case 17:
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_DAYTIME);
                    for (SettingChangeCallback callback : changeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_DAYTIME);
                    }
                    break;
                case 18:
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_NIGHT);
                    for (SettingChangeCallback callback : changeCallbackList.values()) {
                        callback.onSettingChanged(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_NIGHT);
                    }
            }
        }
    }

    /**
     * 获取白天黑夜
     * @return 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     */
    public int getConfigKeyDayNightMode() {
        int dayNightMode;
        String data = getValueFromDB(SettingController.KEY_SETTING_DISPLAY_MODE);
        if (!TextUtils.isEmpty(data)) {
            dayNightMode = switch (data) {
                case SettingController.VALUE_DISPLAY_MODE_DAYTIME -> 17;
                case SettingController.VALUE_DISPLAY_MODE_NIGHT -> 18;
                default -> 16;
            };
        } else {
            dayNightMode = settingAdapter.getConfigKeyDayNightMode();
            switch (dayNightMode) {
                case 16:
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_AUTO);
                    break;
                case 17:
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_DAYTIME);
                    break;
                case 18:
                    settingManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, SettingController.VALUE_DISPLAY_MODE_NIGHT);
            }
        }
        return dayNightMode;
    }

    /**
     * 设置授权状态
     * @param isOneYearPrivacy true：授权一年 false：永不授权
     */
    public void setPrivacyStatus(boolean isOneYearPrivacy) {
        if (isOneYearPrivacy) {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_PRIVACY_STATUS, SettingController.VALUE_PRIVACY_ONE_YEAR);
        } else {
            settingManager.insertOrReplace(SettingController.KEY_SETTING_PRIVACY_STATUS, SettingController.VALUE_PRIVACY_NEVER);
        }
    }

    /**
     * 获取授权状态
     * @return  true：授权一年 false：永不授权
     */
    public boolean getPrivacyStatus() {
        String data = getValueFromDB(SettingController.KEY_SETTING_PRIVACY_STATUS);
        if (TextUtils.isEmpty(data)) {
           data = SettingController.VALUE_PRIVACY_NEVER;
        }
        return SettingController.VALUE_PRIVACY_ONE_YEAR.equals(data);
    }

    /**
     * 设置授权到期时间
     * @param endDateTime 到期时间
     */
    public void setEndDate(String endDateTime) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_PRIVACY_END_DATE, endDateTime);
    }

    /**
     * 获取授权到期时间
     * @return 时间
     */
    public String getEndDate() {
        String endDateTime = getValueFromDB(SettingController.KEY_SETTING_PRIVACY_END_DATE);
        if (TextUtils.isEmpty(endDateTime)) {
            endDateTime = "";
        }
        return endDateTime;
    }

    public ArrayList<SimpleFavoriteItemBean> getSimpleFavoriteList(int type, boolean sorted) {
        return settingAdapter.getSimpleFavoriteList(type, sorted);
    }

    public interface SettingChangeCallback {
        /**
         * @param key 设置项的key值
         * @param value 设置项对应的value值
         */
        void onSettingChanged(String key, String value);
    }
}