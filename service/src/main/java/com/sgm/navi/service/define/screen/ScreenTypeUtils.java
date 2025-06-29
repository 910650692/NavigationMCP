package com.sgm.navi.service.define.screen;

import android.content.res.Configuration;

import com.android.utils.log.Logger;

import lombok.Getter;
import lombok.Setter;

public class ScreenTypeUtils {

    private static final class InstanceHolder {
        private static final ScreenTypeUtils instance = new ScreenTypeUtils();
    }

    public static ScreenTypeUtils getInstance() {
        return InstanceHolder.instance;
    }
    private ScreenType screenType  = ScreenType.SCREEN_FULL;
    @Getter
    @Setter
    private boolean isSRGuideTBTOpen  = false;

    public void setScreenType(Configuration configuration) {
        int screenWidthDp = configuration.screenWidthDp;
        if (0 < screenWidthDp && screenWidthDp <= 800) {
            screenType = ScreenType.SCREEN_1_3;
        } else if (screenWidthDp > 800 && screenWidthDp < 1800) {
            screenType = ScreenType.SCREEN_2_3;
        } else {
            screenType = ScreenType.SCREEN_FULL;
        }
        Logger.d("screen_change_used", screenType);
    }

    public boolean isFullScreen() {
        return screenType == ScreenType.SCREEN_FULL;
    }

    public boolean isOneThirdScreen() {
        return screenType == ScreenType.SCREEN_1_3;
    }

    public boolean isTwoThirdScreen() {
        return screenType == ScreenType.SCREEN_2_3;
    }
}
