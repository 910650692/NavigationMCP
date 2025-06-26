package com.sgm.navi.hmi.utils;

import com.sgm.navi.service.define.screen.ScreenType;
import com.sgm.navi.service.define.utils.BevPowerCarUtils;

public class ScreenTypeUtils {
    private static ScreenType screenType  = ScreenType.SCREEN_FULL;

    public static ScreenType getScreenType(){
        return screenType;
    }

    public static void setScreenType(ScreenType screenTypes) {
        BevPowerCarUtils.getInstance().screenType = screenTypes;
        screenType = screenTypes;
    }
}
