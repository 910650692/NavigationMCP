package com.android.utils.file;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;

import org.json.JSONException;
import org.json.JSONObject;

import java.nio.charset.StandardCharsets;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/12
 */
public class ParseJsonUtils {
    private static final String TAG = ParseJsonUtils.class.getSimpleName();

    private ParseJsonUtils() throws Exception {
        throw new Exception("该对象不允许私自创建");
    }

    /**
     * 解析字节数组为json字符串.
     *
     * @param result 字节数组
     * @return Json字符串
     */
    public static String parseJsonFile(byte[] result) {
        String json = new String(result, StandardCharsets.UTF_8);
        Logger.i(TAG, "json 转换 -> " + json);
        return json;
    }

    /**
     * 解析该路径下的文件为json字符串.
     *
     * @param jsonFilePath 必须是asset目录下
     * @return Json字符串
     */
    public static String parseJsonFile(String jsonFilePath) {
        byte[] result = FileUtils.getInstance().getAssetFileContent(jsonFilePath);
        String json = new String(result, StandardCharsets.UTF_8);
        Logger.i(TAG, "json 转换 -> " + json);
        return json;
    }

    /**
     * 解析该路径下的文件为对象.
     *
     * @param jsonFilePath 必须是asset目录下
     * @return 对象实体
     */
    public static <T> T parseJsonFile(String jsonFilePath, Class<T> clz) {
        byte[] result = FileUtils.getInstance().getAssetFileContent(jsonFilePath);
        String json = new String(result, StandardCharsets.UTF_8);
        Logger.i(TAG, "json 转换 -> " + json);
        return GsonUtils.fromJson(json, clz);
    }

    /**
     * @param key layer_item_info
     */
    public static String getStyleBeanJson(JSONObject jsonObject, String key) {
        if (ConvertUtils.isNull(jsonObject) || ConvertUtils.isNull(key)) {
            return "EMPTY";
        }
        StringBuilder jsonBeanBuild = new StringBuilder();
        try {
            jsonBeanBuild.append(jsonObject.get(key));
        } catch (JSONException e) {
            Logger.e(TAG, "解析出错, key:" + key, e);
        }
        return jsonBeanBuild.toString();
    }
}
