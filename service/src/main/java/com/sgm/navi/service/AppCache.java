package com.sgm.navi.service;

import android.annotation.SuppressLint;
import android.app.ActivityOptions;
import android.app.Application;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;

import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class AppCache {
    private static final String TAG = "AppCache";
    private Context mContext;
    private Application mApplication;
    private boolean mIsFirstOpenMap = true;

    public static AppCache getInstance() {
        return AppCache.Helper.RA;
    }

    private static final class Helper {
        @SuppressLint("StaticFieldLeak")
        private static final AppCache RA = new AppCache();
    }

    /**
     * 打开Map应用自身.
     *
     * @param isNaviDesk  true:地图桌面  false:其他桌面.
     */
    public void openMap(final boolean isNaviDesk) {
        if (null == getInstance().mContext) {
            if (Logger.openLog) {
                Logger.i(TAG, "context is empty");
            }
            return;
        }
        final boolean appForeground = ProcessManager.isAppInForeground();
        if (appForeground) {
            return;
        }

        try {
            if (isNaviDesk) {
                Intent intent = new Intent(Intent.ACTION_MAIN);
                intent.addCategory(Intent.CATEGORY_HOME);
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                mContext.startActivity(intent);
            } else {
                final String appPkgName = AppCache.getInstance().getMContext().getPackageName();
                final PackageManager packageManager = AppCache.getInstance().getMContext().getPackageManager();
                final Intent launcherIntent = packageManager.getLaunchIntentForPackage(appPkgName);
                if (null != launcherIntent) {
                    launcherIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    final ActivityOptions options = ActivityOptions.makeBasic();
                    options.setLaunchDisplayId(0);
                    mContext.startActivity(launcherIntent, options.toBundle());
                } else {
                    Logger.e(TAG, "can't find map hmi");
                }
            }
        } catch (ActivityNotFoundException exception) {
            Logger.e(TAG, "open map error: " + exception.getMessage());
        }
    }

    public boolean isFirstOpenMap() {
        return mIsFirstOpenMap;
    }

    public void setFirstOpenMap(boolean isFirstOpenMap) {
        this.mIsFirstOpenMap = isFirstOpenMap;
    }
}
