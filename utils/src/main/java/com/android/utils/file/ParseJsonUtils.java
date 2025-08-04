package com.android.utils.file;

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
        if (result == null) {
            return null;
        }
        return new String(result, StandardCharsets.UTF_8);
    }

    /**
     * 解析该路径下的文件为json字符串.
     *
     * @param jsonFilePath 必须是asset目录下
     * @return Json字符串
     */
    public static String parseJsonFile(String jsonFilePath) {
        byte[] result = FileUtils.getInstance().getAssetFileContent(jsonFilePath);
        if(result == null){
            return null;
        }
        String json = new String(result, StandardCharsets.UTF_8);
        return json;
    }

    /**
     * 解析该路径下的文件为json字符串.
     *
     * @param jsonFilePath 必须是asset目录下
     * @return Json字符串
     */
    public static String parseHtmlFile(String jsonFilePath) {
        byte[] result = FileUtils.getInstance().getAssetFileContent(jsonFilePath);
        if(result == null){
            return null;
        }
        String html = new String(result, StandardCharsets.UTF_8);
        return html;
    }
}
