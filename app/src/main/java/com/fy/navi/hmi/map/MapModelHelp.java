package com.fy.navi.hmi.map;

import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import kotlin.jvm.Throws;

/**
 * Author: QiuYaWei
 * Date: 2025/3/8
 * Description: [MapModel的帮助类，目的减少MapModel的代码]
 */
public class MapModelHelp {
    private static final String TAG = "MapModelHelp";
    private MapTypeId mapTypeId;
    private SettingPackage settingPackage;
    private MapPackage mapPackage;
    private LayerPackage layerPackage;

    public MapModelHelp(MapTypeId mapTypeId) {
        this.mapTypeId = mapTypeId;
        settingPackage = SettingPackage.getInstance();
        mapPackage = MapPackage.getInstance();
        layerPackage = LayerPackage.getInstance();
    }

    public MapTypeId getMapTypeId() {
        return mapTypeId;
    }

    /***
     * 恢复设置偏好
     */
    public void restoreSetting() {
        // 路况开启恢复
        boolean iShowTmc = settingPackage.getConfigKeyRoadEvent();
        Logger.i(TAG, "restoreUserPreference", "iShowTmc:" + iShowTmc);
        mapPackage.setTrafficStates(mapTypeId, iShowTmc);
        // 收藏是否显示
        settingPackage.hideOrShowFavoriteOnMainMap();
        // 视角
        resetMapAngel();
        // 车标
        restoreCarMode();
    }

    // 恢复底图视角
    private void resetMapAngel() {
        int carMode = settingPackage.getConfigKeyMapviewMode();
        Logger.i(TAG, "resetMapAngel", "carMode:" + carMode);
        switch (carMode) {
            case 0 -> {
                mapPackage.switchMapMode(mapTypeId, MapMode.UP_2D);
            }
            case 1 -> {
                mapPackage.switchMapMode(mapTypeId, MapMode.NORTH_2D);
            }
            case 2 -> {
                mapPackage.switchMapMode(mapTypeId, MapMode.UP_3D);
            }
        }
    }

    // 车标恢复
    private void restoreCarMode() {
        //车标模式, 0: 2D默认车标  1: 3D默认车标 2: 3D骨骼车标  3: 3D车速车标
        int carLogo = settingPackage.getCarMode();
        Logger.i(TAG, "restoreCarMode", "车标模式:" + carLogo);
        switch (carLogo) {
            case 0 -> {
                layerPackage.setCarMode(mapTypeId, CarModeType.CAR_MODEL_TYPE_2D);
            }
            case 3 -> {
                layerPackage.setCarMode(mapTypeId, CarModeType.CAR_MODEL_TYPE_SPEED);
            }
            case 1, 2 -> {
                layerPackage.setCarMode(mapTypeId, CarModeType.CAR_MODEL_TYPE_SKELETON);
            }
            default -> {
                Logger.w(TAG, "不支持的车标类型！");
            }
        }
    }

    // 1.若自动比例尺关闭，则地图默认比例尺 2D:500m, 3D:50m, 允许用户手动调节比例尺与俯仰角
    //2.若自动比例尺开启，比例尺范围设置20-200m
    public void setCruiseScale() {
        boolean isAutoSizeOpen = settingPackage.getAutoScale();
        boolean is3DDegree = settingPackage.getConfigKeyMapviewMode() == 2;
        Logger.i(TAG, "setCruiseScale", "isAutoSizeOpen:" + isAutoSizeOpen, "is3DDegree:" + is3DDegree);
        if (isAutoSizeOpen) {
            mapPackage.setZoomLevel(mapTypeId, 15);
        } else {
            if (is3DDegree) {
                mapPackage.setZoomLevel(mapTypeId, 17);
            } else {
                mapPackage.setZoomLevel(mapTypeId, 14);
            }
        }
    }

    public GeoPoint parseGeoPoint(String geoPointString) {
        Logger.i(TAG, "parseGeoPoint:" + geoPointString);
        if (TextUtils.isEmpty(geoPointString)) throw new NullPointerException("geoPointString is null or empty!");
        Pattern pattern = Pattern.compile("lon=([-\\d.]+), lat=([-\\d.]+)");
        Matcher matcher = pattern.matcher(geoPointString);
        double lon = 0.0;
        double lat = 0.0;
        if (matcher.find()) {
            String lonStr = matcher.group(1);
            String latStr = matcher.group(2);
            if (lonStr != null && latStr != null) {
                try {
                    lon = Double.parseDouble(lonStr);
                    lat = Double.parseDouble(latStr);
                } catch (NumberFormatException e) {
                    Logger.e(TAG, "parseGeoPoint: Invalid number format for GeoPoint string: " + geoPointString);
                }
            } else {
                Logger.e(TAG, "parseGeoPoint: Missing coordinates in GeoPoint string: " + geoPointString);
            }
        } else {
            Logger.e(TAG, "parseGeoPoint: No match found for GeoPoint string: " + geoPointString);
        }
        return new GeoPoint(lon, lat);
    }
}
