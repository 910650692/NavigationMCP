package com.sgm.navi.service.utils;

import com.android.utils.DeviceUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;

public class HudMapConfigUtil {
    private static final String TAG = "HudMapConfigUtil";
    private static final int IS_GB = 1;
    private static final int IS_CLEA = 0;
    private static final int IS_BUICK = 1;
    private static final int IS_CADILLAC = 2;

//    private  int HUD_MAP_SIZE_WIDTH = 260;
    private  int HUD_MAP_SIZE_WIDTH = 300;
    private  int HUD_MAP_SIZE_HEIGHT = 226;


    private  int HUD_MAP_ONE_SIZE_WIDTH = 328;
    private  int HUD_MAP_ONE_SIZE_HEIGHT = 172;

    private HudMapConfigUtil() {
    }

    private static class SingletonHolder {
        private static final HudMapConfigUtil INSTANCE = new HudMapConfigUtil();
    }

    public static HudMapConfigUtil getInstance() {
        return SingletonHolder.INSTANCE;
    }

    public  int getHudMapWidth() {
        if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_BUICK && CalibrationPackage.getInstance().architecture() == IS_CLEA){//buick
            Logger.d(TAG, "HudMapWidth==buick+clea" ,HUD_MAP_SIZE_WIDTH);
            return HUD_MAP_SIZE_WIDTH;
        }else if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_CADILLAC && CalibrationPackage.getInstance().architecture() == IS_GB){//Cadillac
            Logger.d(TAG, "HudMapWidth==cadillac+gb" ,HUD_MAP_SIZE_WIDTH);
            return HUD_MAP_SIZE_WIDTH;
        }else {
            Logger.d(TAG, "HudMapWidth==NONONO");
            return HUD_MAP_ONE_SIZE_WIDTH;//1.0宽度
        }
    }
    public  int getHudMapHeight() {
        if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_BUICK && CalibrationPackage.getInstance().architecture() == IS_CLEA){//buick
            Logger.d(TAG, "HudMapHeight==buick+clea",HUD_MAP_SIZE_HEIGHT);
            return HUD_MAP_SIZE_HEIGHT;
        }else if (DeviceUtils.isCar(AppCache.getInstance().getMContext()) && CalibrationPackage.getInstance().brand() == IS_CADILLAC && CalibrationPackage.getInstance().architecture() == IS_GB){//Cadillac
            Logger.d(TAG, "HudMapHeight==cadillac+gb",HUD_MAP_SIZE_HEIGHT);
            return HUD_MAP_SIZE_HEIGHT;
        }else {
            Logger.d(TAG, "HudMapHeight==NONONO");
            return HUD_MAP_ONE_SIZE_HEIGHT;//1.0高度
        }
    }
}
