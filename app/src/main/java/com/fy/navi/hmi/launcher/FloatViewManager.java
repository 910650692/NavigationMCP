package com.fy.navi.hmi.launcher;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

public class FloatViewManager {
    private static final String TAG = "FloatViewManager";

    private FloatViewManager() {

    }

    private static final class Holder {
        private static final FloatViewManager instance = new FloatViewManager();
    }

    public static FloatViewManager getInstance() {
        return Holder.instance;
    }

    @Nullable
    private LauncherWindowService mWindowService;

    public void bindWindowService(LauncherWindowService service) {
        Logger.i(TAG, "bindWindowService success!");
        this.mWindowService = service;
    }

    public void unBindWindowService() {
        Logger.i(TAG, "unBindWindowService success!");
        this.mWindowService = null;
    }

    public void showOrHideFloatView(boolean isShow) {
        if (!ConvertUtils.isNull(mWindowService)) {
            mWindowService.showOrHideFloatView(isShow);
        }
    }
}
