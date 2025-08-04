package com.android.utils.crash;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Build;


import com.android.utils.log.Logger;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

public class AppCrashRecord implements Thread.UncaughtExceptionHandler {

    private Thread.UncaughtExceptionHandler defaultHandler;

    private static final String TAG = "AppCrashRecord";

    private Context context;

    public AppCrashRecord(Context context) {
        if (context == null) {
            throw new IllegalArgumentException("Context cannot be null");
        }
        this.context = context;
        this.defaultHandler = Thread.getDefaultUncaughtExceptionHandler();
    }

    @Override
    public void uncaughtException(Thread thread, Throwable ex) {
        try {
            if (context == null) {
                return;
            }
            // 收集崩溃信息
            String crashInfo = collectCrashInfo(ex);
            // 保存到本地
            saveToFile(crashInfo);
        } catch (Exception e) {
            Logger.e(TAG, "Error handling crash", e);
        } finally {
            // 交给系统默认处理（可选：关闭应用）
            if (defaultHandler != null) {
                defaultHandler.uncaughtException(thread, ex);
            } else {
                if (Logger.openLog) {
                    Logger.printStackTrace("NaviApp_Exit",true);
                }
                System.exit(1);
            }
        }
    }

    private String collectCrashInfo(Throwable ex) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
        pw.flush();
        // 添加设备信息
        String deviceInfo = "\nDevice Info:\n"
                + "Brand: " + Build.BRAND + "\n"
                + "Model: " + Build.MODEL + "\n"
                + "Android Version: " + Build.VERSION.RELEASE + "\n"
                + "App Version: " + getAppVersion(context) + "\n";

        return sw.toString() + deviceInfo;
    }

    private void saveToFile(String crashInfo) {
        if (context == null) {
            return;
        }
        String fileName = "crash_" + System.currentTimeMillis() + ".log";
        File file = new File(context.getFilesDir(), "crash_logs");
        if (!file.exists()) {
            file.mkdirs();
        }
        File logFile = new File(file, fileName);
        try (FileOutputStream fos = new FileOutputStream(logFile)) {
            fos.write(crashInfo.getBytes());
            Logger.e(TAG, "crash 信息写入成功到 :" + file.getAbsolutePath());
        } catch (Exception exception) {
            Logger.e(TAG, "crash 信息写入失败" + exception.toString());
        }
    }

    private String getAppVersion(Context context) {
        try {
            PackageInfo pInfo = context.getPackageManager()
                    .getPackageInfo(context.getPackageName(), 0);
            return pInfo.versionName + " (" + pInfo.versionCode + ")";
        } catch (PackageManager.NameNotFoundException e) {
            return "Unknown";
        }
    }
}
