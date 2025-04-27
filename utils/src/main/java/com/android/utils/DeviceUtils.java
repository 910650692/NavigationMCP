package com.android.utils;

import android.annotation.SuppressLint;
import android.app.UiModeManager;
import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Rect;
import android.os.Build;
import android.provider.Settings;
import android.telephony.TelephonyManager;
import android.text.TextUtils;
import android.util.Log;

import com.android.utils.log.Logger;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class DeviceUtils {
    protected static Context mApplication;

    /***设备类型***/
    private static int deviceType = -1;
    /**
     * 设备id
     **/
    private static String deviceId;

    /**
     * 获取设备deviceID
     *
     * @return
     */
    public static String getDeviceId() {
        if (TextUtils.isEmpty(deviceId)) {
            deviceId = getMD5(getDeviceInfo());
        }
        return deviceId;
    }

    /**
     * 判断当前设备是不是车机
     *
     * @param context
     * @return
     */
    public static boolean isCar(Context context) {
        return getDeviceType(context) == Configuration.UI_MODE_TYPE_CAR;
    }

    private static String getDeviceInfo() {
        StringBuilder deviceId = new StringBuilder();
        String imei = getIMEI();
        String androidId = getAndroidId();
        String serial = getSerial();
        String deviceUUID = getDeviceUUID();
        deviceId.append(imei).append(androidId).append(serial).append(deviceUUID);
        return deviceId.toString();
    }

    /**
     * 获取IMEI
     *
     * @return
     */
    @SuppressLint("MissingPermission")
    private static String getIMEI() {
        try {
            TelephonyManager tm = (TelephonyManager) mApplication.getSystemService(Context.TELEPHONY_SERVICE);
            return tm.getDeviceId();
        } catch (Exception e) {
            Logger.i("DeviceUtils", e.toString());
        }
        return "";
    }

    /**
     * 获取设备的AndroidId
     *
     * @return
     */
    private static String getAndroidId() {
        try {
            return Settings.Secure.getString(mApplication.getContentResolver(), Settings.Secure.ANDROID_ID);
        } catch (Exception e) {
            Logger.i("DeviceUtils", e.toString());
        }
        return "";
    }

    /**
     * 获取设备序列号
     *
     * @return
     */
    private static String getSerial() {
        try {
            return Build.SERIAL;
        } catch (Exception e) {
            Logger.i("DeviceUtils", e.toString());
        }
        return "";
    }

    /**
     * 获取硬件设备UUID
     * 拼接设备硬件uuid，计算出一个和硬件设备相关的随机数
     *
     * @return
     */
    private static String getDeviceUUID() {
        try {
            String dev = "3883756" +
                    Build.BOARD.length() % 10 +
                    Build.BRAND.length() % 10 +
                    Build.DEVICE.length() % 10 +
                    Build.HARDWARE.length() % 10 +
                    Build.ID.length() % 10 +
                    Build.MODEL.length() % 10 +
                    Build.PRODUCT.length() % 10 +
                    Build.SERIAL.length() % 10;
            return new UUID(dev.hashCode(), Build.SERIAL.hashCode()).toString();
        } catch (Exception e) {
            Logger.i("DeviceUtils", e.toString());
        }
        return UUID.randomUUID().toString();
    }

    /**
     * MD5加密
     *
     * @param info
     * @return
     */
    public static String getMD5(String info) {
        try {
            MessageDigest md5 = MessageDigest.getInstance("MD5");
            md5.update(info.getBytes(StandardCharsets.UTF_8));
            byte[] md5Array = md5.digest();
            return new BigInteger(1, md5Array).toString(16).toUpperCase();
        } catch (NoSuchAlgorithmException e) {
            return "";
        }
    }

    /**
     * 获取设备类型
     *
     * @param context this context
     */
    public static int getDeviceType(Context context) {
        if (deviceType >= 0) return deviceType;
        if (context == null) return deviceType;
        UiModeManager manager = (UiModeManager) context.getSystemService(Context.UI_MODE_SERVICE);
        if (manager == null) return deviceType;
        deviceType = manager.getCurrentModeType();
        Log.i("A_MAP", "getDeviceType deviceType =" + deviceType);
        return deviceType;
    }

    public static Context getApplication() {
        return mApplication;
    }
}
