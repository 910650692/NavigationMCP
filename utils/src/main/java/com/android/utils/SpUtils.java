package com.android.utils;

import android.content.Context;
import android.content.SharedPreferences;

public class SpUtils {
    private static Context mApplication;
    private static final String SP_NAME = "navi_app_sp";
    public static final String SP_KEY_LOG_SWITCH = "log_switch";
    private static SharedPreferences sp;

    public SharedPreferences getSp() {
        if (sp == null) {
            sp = mApplication.getApplicationContext().
                    getSharedPreferences(SP_NAME, Context.MODE_PRIVATE);
        }
        return sp;
    }

    public void init(Context context) {
        mApplication = context;
    }

    public static SpUtils getInstance() {
        return SpUtils.Helper.INSTANCE;
    }

    private static final class Helper {
        public static final SpUtils INSTANCE = new SpUtils();
    }

    private SpUtils() {
    }

    public void putString(String key, String value) {
        getSp().edit().putString(key, value).apply();
    }

    public String getString(String key, String defValue) {
        return getSp().getString(key, defValue);
    }

    public void putInt(String key, int value) {
        getSp().edit().putInt(key, value).apply();
    }

    public int getInt(String key, int defValue) {
        return getSp().getInt(key, defValue);
    }

    public void putBoolean(String key, boolean value) {
        getSp().edit().putBoolean(key, value).apply();
    }

    public boolean getBoolean(String key, boolean defValue) {
        return getSp().getBoolean(key, defValue);
    }

    public void remove(String key) {
        getSp().edit().remove(key).apply();
    }

    public void clear() {
        getSp().edit().clear().apply();
    }
}