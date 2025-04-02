package com.fy.navi.hmi.map;

import android.os.CountDownTimer;
import android.text.TextUtils;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.calibration.PowerType;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author : QiuYaWei
 * @version $Revision.*$
 * Date: 2025/3/8
 * Description: [MapModel的帮助类，目的减少MapModel的代码]
 */
public class MapModelHelp {
    private static final String TAG = "MapModelHelp";
    private final MapType mMapTypeId;
    private final SettingPackage mSettingPackage;
    private final MapPackage mMapPackage;
    private final LayerPackage mLayerPackage;
    private final SignalPackage mSignalPackage;
    private final NaviPackage mNaviPackage;
    private final CalibrationPackage mCalibrationPackage;
    private boolean mIsScheduling = false;
    // 导航过程中每分钟上传一次电量给API
    private final CountDownTimer mCountDownTimer = new CountDownTimer(Long.MAX_VALUE, 60 * 1000) {
        @Override
        public void onTick(final long millisUntilFinished) {
            mIsScheduling = true;
            Logger.i(TAG, "onTick set battery!");
            uploadBattery();
        }

        @Override
        public void onFinish() {
            mIsScheduling = false;
            Logger.i(TAG, "CountDownTimer --- onFinish !");
        }
    };

    public MapModelHelp(final MapType mapTypeId) {
        this.mMapTypeId = mapTypeId;
        mSettingPackage = SettingPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSignalPackage = SignalPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
    }

    public MapType getMapTypeId() {
        return mMapTypeId;
    }

    /***
     * 恢复设置偏好
     */
    public void restoreSetting() {
        // 路况开启恢复
        final boolean iShowTmc = mSettingPackage.getConfigKeyRoadEvent();
        Logger.i(TAG, "restoreUserPreference", "iShowTmc:" + iShowTmc);
        mMapPackage.setTrafficStates(mMapTypeId, iShowTmc);
        // 收藏是否显示
        mSettingPackage.hideOrShowFavoriteOnMainMap();
        // 视角
        resetMapAngel();
        // 车标
        restoreCarMode();
    }

    /***
     * 恢复底图视角
     */
    private void resetMapAngel() {
        final int carMode = mSettingPackage.getConfigKeyMapviewMode();
        Logger.i(TAG, "resetMapAngel", "carMode:" + carMode);
        switch (carMode) {
            case 0 -> {
                mMapPackage.switchMapMode(mMapTypeId, MapMode.UP_2D);
            }
            case 1 -> {
                mMapPackage.switchMapMode(mMapTypeId, MapMode.NORTH_2D);
            }
            case 2 -> {
                mMapPackage.switchMapMode(mMapTypeId, MapMode.UP_3D);
            }
            default -> {
                Logger.e(TAG, "resetMapAngel failed!");
            }
        }
    }

    /***
     * 车标恢复
     */
    private void restoreCarMode() {
        //车标模式, 0: 2D默认车标  1: 3D默认车标 2: 3D骨骼车标  3: 3D车速车标
        final CarModeType carLogo = mSettingPackage.getCarMode();
        Logger.i(TAG, "restoreCarMode", "车标模式:" + carLogo);
        switch (carLogo) {
            case CAR_MODEL_TYPE_2D -> {
                mLayerPackage.setCarMode(mMapTypeId, CarModeType.CAR_MODEL_TYPE_2D);
            }
            case CAR_MODEL_TYPE_SPEED -> {
                mLayerPackage.setCarMode(mMapTypeId, CarModeType.CAR_MODEL_TYPE_SPEED);
            }
            case CAR_MODEL_TYPE_SKELETON -> {
                mLayerPackage.setCarMode(mMapTypeId, CarModeType.CAR_MODEL_TYPE_SKELETON);
            }
            default -> {
                Logger.w(TAG, "不支持的车标类型！");
            }
        }
    }


    /***
     * 1.若自动比例尺关闭，则地图默认比例尺 2D:500m, 3D:50m, 允许用户手动调节比例尺与俯仰角
     * 2.若自动比例尺开启，比例尺范围设置20-200m
     */
    public void setCruiseScale() {
        final boolean isAutoSizeOpen = mSettingPackage.getAutoScale();
        final boolean is3DDegree = mSettingPackage.getConfigKeyMapviewMode() == 2;
        Logger.i(TAG, "setCruiseScale", "isAutoSizeOpen:" + isAutoSizeOpen, "is3DDegree:" + is3DDegree);
        if (isAutoSizeOpen) {
            mMapPackage.setZoomLevel(mMapTypeId, 15);
        } else {
            if (is3DDegree) {
                mMapPackage.setZoomLevel(mMapTypeId, 17);
            } else {
                mMapPackage.setZoomLevel(mMapTypeId, 14);
            }
        }
    }

    /***
     * 把字符串解析成经纬度坐标
     * @param geoPointString
     * @return GeoPoint
     */
    public GeoPoint parseGeoPoint(final String geoPointString) {
        Logger.i(TAG, "parseGeoPoint:" + geoPointString);
        if (TextUtils.isEmpty(geoPointString)) {
            throw new NullPointerException("geoPointString is null or empty!");
        }
        final Pattern pattern = Pattern.compile("lon=([-\\d.]+), lat=([-\\d.]+)");
        final Matcher matcher = pattern.matcher(geoPointString);
        double lon = 0.0;
        double lat = 0.0;
        if (matcher.find()) {
            final String lonStr = matcher.group(1);
            final String latStr = matcher.group(2);
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

    /***
     * 更新电量
     */
    public void uploadBattery() {
        if (!DeviceUtils.isCar(AppContext.getInstance().getMContext())) {
            Logger.i(TAG, "uploadBattery", "不是汽车，无需上传！");
            return;
        }
        if (mCalibrationPackage.powerType() != PowerType.E_VEHICLE_ENERGY_ELECTRIC) {
            Logger.w(TAG, "不是纯电动汽车，无需上传电量！");
            return;
        }
        if (mSignalPackage == null || mNaviPackage == null) {
            return;
        }
        final float currentBattery = mSignalPackage.getBatteryEnergy(); // 获取剩余电量
        BevPowerCarUtils.getInstance().initlialHVBattenergy = currentBattery;
        Logger.i(TAG, "currentBattery:" + currentBattery);
        mNaviPackage.updateBatteryInfo();
    }

    /***
     * 如果导航开始就开始循环上传电量
     */
    public void startScheduleUploadBattery() {
        if (mCalibrationPackage.powerType() != PowerType.E_VEHICLE_ENERGY_ELECTRIC) {
            Logger.w(TAG, "startScheduleUploadBattery", "不是纯电动汽车，无需上传电量！");
            return;
        }
        if (!mIsScheduling) {
            mIsScheduling = true;
            mCountDownTimer.start();
            Logger.w(TAG, "schedule upload battery start success!");
        } else {
            Logger.w(TAG, "schedule already start not restart again!");
        }
    }

    /***
     * 如果导航结束，停止循环上传电量
     */
    public void stopScheduleUploadBattery() {
        if (mIsScheduling) {
            mIsScheduling = false;
            mCountDownTimer.cancel();
            Logger.w(TAG, "stop schedule uploadBattery success!");
        } else {
            Logger.w(TAG, "schedule not ongoing, not need cancel!");
        }
    }

    /***
     * 开始导航立即开启定时任务上传电量
     * @param naviStatus
     */
    public void onNaviStatusChange(final String naviStatus) {
        if (TextUtils.equals(naviStatus, NaviStatus.NaviStatusType.NAVING)) {
            startScheduleUploadBattery();
        } else {
            stopScheduleUploadBattery();
        }
    }

    /***
     * 对象销毁的时候释放不需要的资源
     */
    public void unInit() {
        Logger.i(TAG, "unInit start!");
        if (mIsScheduling) {
            mCountDownTimer.cancel();
        }
    }
}
