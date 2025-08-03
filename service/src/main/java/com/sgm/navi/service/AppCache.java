package com.sgm.navi.service;

import android.annotation.SuppressLint;
import android.app.Application;
import android.content.Context;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class AppCache {
    private static final String TAG = "AppCache";
    private Context mContext;
    private Application mApplication;
    private boolean mIsFirstOpenMap = true;
    private boolean mHandCarInterconnection = true;

    public static AppCache getInstance() {
        return AppCache.Helper.RA;
    }

    private static final class Helper {
        @SuppressLint("StaticFieldLeak")
        private static final AppCache RA = new AppCache();
    }

    public boolean isFirstOpenMap() {
        return mIsFirstOpenMap;
    }

    public void setFirstOpenMap(boolean isFirstOpenMap) {
        this.mIsFirstOpenMap = isFirstOpenMap;
    }
}
