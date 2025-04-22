package com.android.utils;

import android.content.Context;

import patac.manager.PatacServiceManager;
import patac.manager.PatacServiceNotConnectedException;
import patac.manager.vehicle.PatacVehicleManager;

public class DevicesIdUtil {
    protected static Context mApplication;
    private PatacServiceManager mPatacServiceManager;

    public static DevicesIdUtil getInstance() {
        return Helper.INSTANCE;
    }

    private static final class Helper {
        public static final DevicesIdUtil INSTANCE = new DevicesIdUtil();
    }

    public DevicesIdUtil() {
        mPatacServiceManager = PatacServiceManager.newInstance(mApplication);
    }

    /**
     * 用于加密Vin的获取devicesId
     * @return id
     */
    public String getDeviceId() {
        try {
            final PatacVehicleManager vehicleManager
                    = (PatacVehicleManager) mPatacServiceManager.getPatacManager(PatacServiceManager.PATAC_VEHICLE_SERVICE);
            return vehicleManager.getVinId();
        } catch (PatacServiceNotConnectedException e) {
            throw new RuntimeException(e);
        }
    }
}
