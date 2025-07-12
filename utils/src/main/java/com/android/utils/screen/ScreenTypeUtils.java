package com.android.utils.screen;

import android.content.res.Configuration;
import android.util.DisplayMetrics;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.util.concurrent.ConcurrentHashMap;

public class ScreenTypeUtils {

    private static final class InstanceHolder {
        private static final ScreenTypeUtils instance = new ScreenTypeUtils();
    }

    public static ScreenTypeUtils getInstance() {
        return InstanceHolder.instance;
    }

    private ScreenType screenType = ScreenType.SCREEN_FULL;
    private boolean isSRGuideTBTOpen = false;

    private final ConcurrentHashMap<String, SplitScreenChangeListener> mSplitScreenChangeListeners;

    public ScreenTypeUtils() {
        mSplitScreenChangeListeners = new ConcurrentHashMap<>();
    }

    public void setScreenType(Configuration configuration) {
        Logger.d("screen_change_used", configuration.screenWidthDp, "screenType", screenType);
        ScreenType newScreenType = calculateScreenType(configuration.screenWidthDp);
        if (screenType != newScreenType) {
            screenType = newScreenType;
            Logger.d("screen_change_used", screenType);
            callBackScreenChange();
        } else {
            Logger.d("screen_change_used", screenType + " 没有变化");
        }
    }

    public void changeSkinTheme() {
        Logger.d("screen_change_used", "分屏换肤");
        callBackScreenChangeTheme();
    }

    private void callBackScreenChange() {
        for (SplitScreenChangeListener listener : mSplitScreenChangeListeners.values()) {
            if (listener != null) {
                listener.onSplitScreenChanged();
            }
        }
    }

    private void callBackScreenChangeTheme() {
        if (ConvertUtils.isEmpty(mSplitScreenChangeListeners)) return;
        for (SplitScreenChangeListener listener : mSplitScreenChangeListeners.values()) {
            if (listener != null) listener.applySplitTheme();
        }
    }

    public ScreenType calculateScreenType(int screenWidth) {
        if (0 < screenWidth && screenWidth <= 800) {
            return ScreenType.SCREEN_1_3;
        } else if (screenWidth > 800 && screenWidth < 1800) {
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

    public boolean isScreenChange(Configuration configuration) {
        return ScreenType.SCREEN_FULL != calculateScreenType(configuration.screenWidthDp);
    }

    public void checkScreenType(DisplayMetrics displayMetrics) {
        if (displayMetrics == null) {
            Logger.e("screen_change_used", screenType);
            return;
        }
        int widthPixels = ScreenUtils.Companion.getInstance().px2dp(displayMetrics.widthPixels);
        Logger.d("screen_change_used", widthPixels);
        ScreenType activityScreenType = calculateScreenType(widthPixels);
        if (activityScreenType != screenType) {
            Logger.e("screen_change_used", "发生变化, onConfigurationChanged没有回调导致");
            screenType = activityScreenType;
            callBackScreenChange();
        } else {
            Logger.d("screen_change_used", "没有变化");
        }
    }

    public ScreenType getScreenType() {
        return screenType;
    }

    public void setScreenType(ScreenType screenType) {
        this.screenType = screenType;
    }

    public boolean isSRGuideTBTOpen() {
        return isSRGuideTBTOpen;
    }

    public void setSRGuideTBTOpen(boolean SRGuideTBTOpen) {
        isSRGuideTBTOpen = SRGuideTBTOpen;
    }

    public void addSplitScreenChangeListener(String key, SplitScreenChangeListener listener) {
        if (!mSplitScreenChangeListeners.containsKey(key)) {
            mSplitScreenChangeListeners.put(key, listener);
        }
    }

    public void removeSplitScreenChangeListener(String key) {
        mSplitScreenChangeListeners.remove(key);
    }

    public interface SplitScreenChangeListener {
        void onSplitScreenChanged();

        void applySplitTheme();
    }
}
