package com.android.utils;

import android.content.Context;

import patac.manager.PatacServiceManager;
import patac.manager.PatacServiceNotConnectedException;
import patac.manager.vehicle.PatacVehicleManager;

public final class DevicesIdUtil {
    private static Context mApplication;
    private PatacServiceManager mPatacServiceManager;

    public static DevicesIdUtil getInstance() {
        return Helper.INSTANCE;
    }

    private static final class Helper {
        public static final DevicesIdUtil INSTANCE = new DevicesIdUtil();
    }

    private DevicesIdUtil() {

    }

    /**
     * 初始化context
     * @param context context
     */
    public void init(final Context context) {
        mApplication = context;
    }

    /**
     * 反初始化
     */
    public void unInit() {
        mApplication = null;
    }

    /**
     * 用于加密Vin的获取devicesId
     * @return id
     */
    public String getDeviceId() {
        if (!DeviceUtils.isCar(mApplication)){
            return "";
        }
        mPatacServiceManager = PatacServiceManager.newInstance(mApplication);
        try {
            final PatacVehicleManager vehicleManager
                    = (PatacVehicleManager) mPatacServiceManager.getPatacManager(PatacServiceManager.PATAC_VEHICLE_SERVICE);
            return vehicleManager.getVinId();
        } catch (PatacServiceNotConnectedException e) {
            throw new RuntimeException(e);
        }
    }
}
