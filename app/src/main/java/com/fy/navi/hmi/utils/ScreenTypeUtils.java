package com.fy.navi.hmi.utils;

import com.fy.navi.service.define.screen.ScreenType;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormat;

public class ScreenTypeUtils {
    private static ScreenType screenType  = ScreenType.SCREEN_FULL;

    public static ScreenType getScreenType(){
        return screenType;
    }

    public static void setScreenType(ScreenType screenTypes) {
        screenType = screenTypes;
    }
}
