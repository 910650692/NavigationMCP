package com.android.utils;

import android.content.Context;
import android.text.TextUtils;

import com.android.utils.gson.GsonUtils;

import java.io.IOException;
import java.io.InputStream;

import kotlin.text.Charsets;

/**
 * Author: QiuYaWei
 * Date: 2025/3/13
 * Description: [在这里描述文件功能]
 */
public class MockUtil {
    public static  <T> T getObjectFromAsset(Context context, String fileName, Class<T> cls) {
        String json;
        try {
            InputStream inputStream = context.getAssets().open(fileName);
            int size = inputStream.available();
            byte[] buffer = new byte[size];
            inputStream.read(buffer);
            inputStream.close();
            json = new String(buffer, Charsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        if (TextUtils.isEmpty(json)) return null;
        else {
            return GsonUtils.fromJson(json, cls);
        }
    }
}
