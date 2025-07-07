package com.sgm.navi.service.utils;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;

public class HudMapConfigUtil {
    private static final String TAG = "HudMapConfigUtil";
    private static final int IS_GB = 1;
    private static final int IS_CLEA = 0;
    private static final int IS_BUICK = 1;
    private static final int IS_CADILLAC = 2;

    private HudMapConfigUtil() {
    }

    private static class SingletonHolder {
        private static final HudMapConfigUtil INSTANCE = new HudMapConfigUtil();
    }

    public static HudMapConfigUtil getInstance() {
        return SingletonHolder.INSTANCE;
    }

    public static int getHudMapWidth() {
        if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_BUICK && CalibrationPackage.getInstance().architecture() == IS_CLEA){//buick
            Logger.d(TAG, "HudMapWidth==buick+clea");
            return 328;
        }else if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_CADILLAC && CalibrationPackage.getInstance().architecture() == IS_GB){//Cadillac
            Logger.d(TAG, "HudMapWidth==cadillac+gb");
            return 328;
        }else {
            Logger.d(TAG, "HudMapWidth==NONONO");
            return 328;
        }
    }
    public static int getHudMapHeight() {
        if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_BUICK && CalibrationPackage.getInstance().architecture() == IS_CLEA){//buick
            Logger.d(TAG, "HudMapHeight==buick+clea");
            return 226;
        }else if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_CADILLAC && CalibrationPackage.getInstance().architecture() == IS_GB){//Cadillac
            Logger.d(TAG, "HudMapHeight==cadillac+gb");
            return 226;
        }else {
            Logger.d(TAG, "HudMapHeight==NONONO");
            return 172;
        }
    }
}
