package com.fy.navi.burypoint.constant;

import android.util.Log;

import androidx.annotation.NonNull;

import java.lang.reflect.Method;

/**
 * data track config class
 * use for production environment validate
 *
 * @author RanKang
 * @version "$Revision 1.0$"
 */
public enum DataTrackerConfig {
    /**
     * 生产环境埋点配置
     */
    PRODUCTION("A00000014", "SV00000088", "S00000008"),

    /**
     * 开发环境埋点配置
     */
    DEVELOPMENT("A00000141", "SV00000544", "S00000282");
    private static final int PRODUCTION_ENVIR_FLAG = 1;
    private static final int OTHER_ENVIR_FLAG = 0;

    private static final String TAG = "DataTrackerConfig";

    private static Method getStringMethod = null;

    private final String appId;
    private final String sVid;
    private final String sId;

    DataTrackerConfig(String appId, String vid, String id) {
        this.appId = appId;
        this.sVid = vid;
        this.sId = id;
    }


    public String getAppId() {
        return appId;
    }

    public String getsVid() {
        return sVid;
    }

    public String getsId() {
        return sId;
    }

    /**
     * is production environment or not
     * 1: production else other environment
     */
    public static boolean isProductionEnvironment() {
        return getInt(BuryConstant.Property.PRODUCTION_SYSTEM_PROPERTY_NAME, OTHER_ENVIR_FLAG)
                == PRODUCTION_ENVIR_FLAG;
//        return true;
    }

    public static int getInt(String key, int def) {
        try {
            if (getStringMethod == null) {
                getStringMethod = Class.forName(BuryConstant.Property.CLASS_NAME).getMethod("getInt", String.class, String.class);
                getStringMethod.setAccessible(true);
            }
            // 调用 getStringMethod 并缓存结果
            Object result = getStringMethod.invoke(null, key, def);

            // 检查结果是否为 null
            if (result == null) {
                return def;
            }

            // 确保结果可以安全地转换为 int
            if (result instanceof Integer) {
                return (int) result;
            } else {
                throw new IllegalArgumentException("Result is not an instance of Integer: " + result.getClass());
            }
        } catch (Exception e) {
            Log.d(TAG, Log.getStackTraceString(e));
            return def;
        }
    }

    /**get current configuration of data tracker*/
    @NonNull
    public static DataTrackerConfig currentConfig() {
//        if (isProductionEnvironment()) {
//            return PRODUCTION;
//
//        }
        return PRODUCTION;
    }
}
