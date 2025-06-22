package com.sgm.navi.hmi.utils;

import com.sgm.navi.service.define.screen.ScreenType;

public class ScreenTypeUtils {
    private static ScreenType screenType  = ScreenType.SCREEN_FULL;

    public static ScreenType getScreenType(){
        return screenType;
    }

    public static void setScreenType(ScreenType screenTypes) {
        screenType = screenTypes;
    }
}
