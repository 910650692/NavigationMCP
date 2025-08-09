package com.android.utils;


import android.content.Context;
import android.content.res.Configuration;
import android.util.DisplayMetrics;

import com.android.utils.log.Logger;

import java.util.concurrent.ConcurrentHashMap;


public class ScreenTypeUtils {

    private Context mContext;

    private static final class InstanceHolder {
        private static final ScreenTypeUtils instance = new ScreenTypeUtils();
    }

    public static ScreenTypeUtils getInstance() {
        return InstanceHolder.instance;
    }

    //(ScreenTypeUtils.java:116) checkScreenType() -> 2394
    //(ScreenTypeUtils.java:68) setScreenType() -> 1582
    //(ScreenTypeUtils.java:68) setScreenType() -> 794

    //(ScreenTypeUtils.java:116) checkScreenType() -> 2179
    //(ScreenTypeUtils.java:68) setScreenType() -> 1439
    //(ScreenTypeUtils.java:68) setScreenType() -> 723

    public void init(Context context) {
        mContext = context;
        DisplayMetrics dm = mContext.getResources().getDisplayMetrics();
        int densityDpi = dm.densityDpi;
        Logger.d("DPI",String.valueOf(densityDpi));
        is557CarMode = 200 == densityDpi;
        carWidthOneThree = is557CarMode ? 993 : 850;
        carWidthTwoThree = is557CarMode ? 1978 : 1691;
        carWidthFull = is557CarMode ? 2992 : 2560;
        carHeight = is557CarMode ? 1299 : 1440;
        toastXOneThree = is557CarMode ? 1000 : 855;
        toastXTwoThree = is557CarMode ? 500 : 435;
    }

    private int carWidthOneThree = 850;
    private int carWidthTwoThree = 1690;
    private  int carWidthFull = 2560;
    private  int carHeight = 1440;
    private boolean is557CarMode = false;

    public int getToastXTwoThree() {
        return toastXTwoThree;
    }

    public int getToastXOneThree() {
        return toastXOneThree;
    }

    private  int toastXOneThree = 855;
    private  int toastXTwoThree = 435;

    public int getCarWidthOneThree() {
        return carWidthOneThree;
    }

    public int getCarWidthTwoThree() {
        return carWidthTwoThree;
    }

    public int getCarWidthFull() {
        return carWidthFull;
    }

    public int getCarHeight() {
        return carHeight;
    }

    private ScreenType screenType  = ScreenType.SCREEN_FULL;

    private final ConcurrentHashMap<String, SplitScreenChangeListener> mSplitScreenChangeListeners;

    public ScreenTypeUtils() {
        mSplitScreenChangeListeners = new ConcurrentHashMap<>();
    }

    public void setScreenType(Configuration configuration) {
        Logger.d("screen_change_used", configuration.screenWidthDp);
        ScreenType newScreenType = calculateScreenType(configuration.screenWidthDp);
        if (screenType != newScreenType) {
            screenType = newScreenType;
            Logger.d("screen_change_used", screenType);
            callBackScreenChange();
        } else {
            Logger.d("screen_change_used", screenType + " 没有变化");
        }
    }

    private void callBackScreenChange() {
        for (SplitScreenChangeListener listener : mSplitScreenChangeListeners.values()) {
            if (listener != null) {
                listener.onSplitScreenChanged();
            }
        }
    }

    public ScreenType calculateScreenType(int width) {
        int screenWidth = ScreenUtils.Companion.getInstance().dp2px(width);
        if (0 < screenWidth && screenWidth <= carWidthOneThree + 100) {
            return ScreenType.SCREEN_1_3;
        } else if (screenWidth > carWidthOneThree + 100 && screenWidth < carWidthTwoThree + 100) {
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

    public boolean is557CarMode() {
        DisplayMetrics dm = mContext.getResources().getDisplayMetrics();
        int densityDpi = dm.densityDpi;
        Logger.d("DPI",String.valueOf(densityDpi));
        return is557CarMode;
    }

    public void addSplitScreenChangeListener(String key, SplitScreenChangeListener listener) {
        if (!mSplitScreenChangeListeners.containsKey(key)){
            mSplitScreenChangeListeners.put(key, listener);
        }
    }

    public void removeSplitScreenChangeListener(String key) {
        mSplitScreenChangeListeners.remove(key);
    }

    public interface SplitScreenChangeListener {
        void onSplitScreenChanged();
    }
}
