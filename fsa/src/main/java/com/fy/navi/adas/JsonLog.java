package com.fy.navi.adas;

import android.annotation.SuppressLint;
import android.icu.text.SimpleDateFormat;
import android.util.Log;

import com.fy.navi.service.AppContext;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * JsonLog
 */
public final class JsonLog {
    //本类TAG
    private static final String TAG = "JsonLog";
    //"\n"为换行符
    private static final String LINE_SEPARATOR = System.getProperty("line.separator");
    //线程池
    private static ExecutorService mExecutorService;

    //防止构造方法创建实例
    private JsonLog() {}

    /**
     * 打印线
     * @param tag   TAG
     * @param isTop 是否打印上边框
     */
    private static void printLine(final String tag, final boolean isTop) {
        // 此类仅在本地调试时使用，且因为格式需要所以使用原生的log
        if (isTop) {
            Log.d(tag, "╔═══════════════════════════════════════════════════════════════════════════════════════");
        } else {
            Log.d(tag, "╚═══════════════════════════════════════════════════════════════════════════════════════");
        }
    }

    /**
     * 打印json
     * @param tag   TAG
     * @param json  json
     */
    public static void print(final String tag, final String json) {
        if (mExecutorService == null) {
            mExecutorService = Executors.newSingleThreadExecutor();
        }
        mExecutorService.submit(() -> {
            final String message = jsonFormat(json);
            printLine(tag, true);
            final String[] lines = message.split(LINE_SEPARATOR);
            for (String line : lines) {
                // 此类仅在本地调试时使用，且因为格式需要所以使用原生的log
                Log.d(tag, "║ " + line);
            }
            printLine(tag, false);
        });
    }

    /**
     * 保存json到缓存目录
     *
     * @param json json
     * @param fileName 文件名
     */
    public static void saveJsonToCache(final String json, final String fileName) {
        saveJsonToCache(json, fileName, null);
    }

    /**
     * 保存json到缓存目录
     *
     * @param json json
     * @param fileName 文件名
     * @param tag   TAG
     */
    @SuppressLint("SimpleDateFormat")
    public static void saveJsonToCache(final String json, final String fileName, final String tag) {
        if (mExecutorService == null) {
            mExecutorService = Executors.newSingleThreadExecutor();
        }
        mExecutorService.submit(() -> {
            String message = jsonFormat(json);
            final SimpleDateFormat dateFormat = new SimpleDateFormat("MM-dd HH:mm:ss.SSS");
            final String currentTime = dateFormat.format(new Date());
            if (tag != null) {
                message = LINE_SEPARATOR + currentTime + " " + tag + LINE_SEPARATOR + message;
            } else {
                message = LINE_SEPARATOR + currentTime + " " + TAG + LINE_SEPARATOR + message;
            }
            final File cacheDir = AppContext.getInstance().getMContext().getCacheDir();
            final File file = new File(cacheDir, fileName);
            try (FileWriter fileWriter = new FileWriter(file, true)) {
                fileWriter.write(message);
            } catch (IOException e) {
                // 此类仅在本地调试时使用，且因为格式需要所以使用原生的log
                Log.e(TAG, "Error saving JSON data to cache file: " + file.getAbsolutePath(), e);
            }
        });
    }

    /**
     * 格式化json
     *
     * @param json json
     * @return 格式化后的json
     */
    private static String jsonFormat(final String json) {
        String message;
        try {
            if (json.startsWith("{")) {
                final JSONObject jsonObject = new JSONObject(json);
                message = jsonObject.toString(4);
            } else if (json.startsWith("[")) {
                final JSONArray jsonArray = new JSONArray(json);
                message = jsonArray.toString(4);
            } else {
                message = json;
            }
        } catch (JSONException e) {
            message = json;
        }
        return message;
    }
}
