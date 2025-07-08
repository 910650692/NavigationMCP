package com.sgm.navi.service.define.screen;

import android.content.res.Configuration;
import android.util.DisplayMetrics;

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
    @Getter
    private ScreenType screenType  = ScreenType.SCREEN_FULL;
    @Getter
    @Setter
    private boolean isSRGuideTBTOpen  = false;

    public void setScreenType(Configuration configuration) {
        screenType = calculateScreenType(configuration);
        Logger.d("screen_change_used", screenType);
    }

    public ScreenType calculateScreenType(Configuration configuration) {
        int screenWidthDp = configuration.screenWidthDp;
        if (0 < screenWidthDp && screenWidthDp <= 800) {
            return ScreenType.SCREEN_1_3;
        } else if (screenWidthDp > 800 && screenWidthDp < 1800) {
            return ScreenType.SCREEN_2_3;
        } else {
            return ScreenType.SCREEN_FULL;
        }
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

    public void isSameScreenType(DisplayMetrics displayMetrics) {
        if (displayMetrics == null) {
            Logger.e("screen_change_used", screenType);
            return;
        }
        ScreenType activityScreenType;
        int widthPixels = displayMetrics.widthPixels;
        if (0 < widthPixels && widthPixels <= 800) {
            activityScreenType = ScreenType.SCREEN_1_3;
        } else if (widthPixels > 800 && widthPixels < 1800) {
            activityScreenType = ScreenType.SCREEN_2_3;
        } else {
            activityScreenType = ScreenType.SCREEN_FULL;
        }

        if (activityScreenType != screenType) {
            Logger.e("screen_change_used", "onConfigurationChanged 保存的和实际上的不一致");
        }
    }
}
