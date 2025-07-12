package com.android.utils;

import android.content.Context;

import com.android.utils.file.FileUtils;
import com.android.utils.log.Logger;

import com.android.utils.screen.ScreenUtils;
import com.android.utils.thread.ThreadManager;

/**
 * @Introduce: .
 * @Author: lvww
 * @Date: 2023/11/13
 * @Description :
 */
public class UtilsManager {
    public static void init(Context context) {
        ThreadManager.getInstance().initThreadPool(context);
        FileUtils.getInstance().initFile(context);
        NetWorkUtils.Companion.getInstance().init(context);
        OkHttpUtils.Companion.getInstance().init(context);
        ToastUtils.Companion.getInstance().init(context);
        ScreenUtils.Companion.getInstance().init(context);
        ResourceUtils.Companion.getInstance().init(context);
        DeviceUtils.mApplication = context;
        SpUtils.getInstance().init(context);
        Logger.switchLog(BuildConfig.DEBUG ||
                SpUtils.getInstance().getBoolean(SpUtils.SP_KEY_LOG_SWITCH, false));
        Logger.i("UtilsManager", "init");
    }

    public static void clearCache() {
        ThreadManager.getInstance().closePool();
        FileUtils.getInstance().close();
        NetWorkUtils.Companion.getInstance().clearCache();
        ToastUtils.Companion.getInstance().destroy();
        ScreenUtils.Companion.getInstance().clearCache();
        ResourceUtils.Companion.getInstance().clearCache();
        DeviceUtils.mApplication = null;
    }
}
