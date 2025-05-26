package com.fy.navi.service.adapter.layer.bls.texture;

import android.text.TextUtils;

import com.android.utils.file.ParseJsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.bls.style.BaseStyleAdapter;
import com.fy.navi.service.define.map.MapType;

import java.io.File;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 纹理样式管理类
 * 1.解析 .json文件
 * 2.解析 .xml文件
 */
public final class TextureStylePoolManager {

    private TextureStylePoolManager() {
    }

    public static TextureStylePoolManager get() {
        return TextureStylePoolManager.Holder._INSTANCE;
    }

    private static final class Holder {
        public static final TextureStylePoolManager _INSTANCE = new TextureStylePoolManager();
    }

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private static final String JSON = ".json";
    private static final String HTML = ".xml";

    private final ConcurrentHashMap<String, String> allStyleJson = new ConcurrentHashMap<>();

    public String getLayerStyle(MapType mapType, LayerItem item, BaseStyleAdapter styleAdapter) {
        String styleJson = null;
        String jsonPathName = styleAdapter.provideLayerItemStyleJson(item);
        if (!TextUtils.isEmpty(jsonPathName)) {
            if (!jsonPathName.endsWith(JSON)) {
                jsonPathName = jsonPathName + JSON;
            }
            styleJson = allStyleJson.get(jsonPathName);
            if (TextUtils.isEmpty(styleJson)) {
                String jsonFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
                        .append(mapType.getMapType())
                        .append(File.separator)
                        .append(jsonPathName).toString();
                styleJson = ParseJsonUtils.parseJsonFile(jsonFilePath);
                if (TextUtils.isEmpty(styleJson)) {
                    jsonFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
                            .append(MapType.MAIN_SCREEN_MAIN_MAP.getMapType())
                            .append(File.separator)
                            .append(jsonPathName).toString();
                    styleJson = ParseJsonUtils.parseJsonFile(jsonFilePath);
                    Logger.e(TAG, "未配置样式, 采用主屏默认样式 :");
                }
                if (!TextUtils.isEmpty(styleJson)) {
                    allStyleJson.put(jsonPathName, styleJson);
                }
                Logger.d(TAG, " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                        + "\n 使用自定义的样式配置文件 : " + jsonFilePath);
            } else {
                Logger.d(TAG, " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                        + "\n 使用缓存的样式配置文件  ");
            }
        }
        return styleJson;
    }

    public String getLayerHtmlStyle(MapType mapType, LayerItem item, BaseStyleAdapter styleAdapter) {
        String styleHtml = null;
        String jsonPathName = styleAdapter.provideLayerItemStyleJson(item);
        if (!TextUtils.isEmpty(jsonPathName)) {
            if (!jsonPathName.endsWith(HTML)) {
                jsonPathName = jsonPathName + HTML;
            }
            styleHtml = allStyleJson.get(jsonPathName);
            if (TextUtils.isEmpty(styleHtml)) {
                String jsonFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
                        .append(mapType.getMapType())
                        .append(File.separator)
                        .append(jsonPathName).toString();
                styleHtml = ParseJsonUtils.parseJsonFile(jsonFilePath);
                if (TextUtils.isEmpty(styleHtml)) {
                    jsonFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
                            .append(MapType.MAIN_SCREEN_MAIN_MAP.getMapType())
                            .append(File.separator)
                            .append(jsonPathName).toString();
                    styleHtml = ParseJsonUtils.parseJsonFile(jsonFilePath);
                    Logger.e(TAG, "未配置样式, 采用主屏默认样式 :");
                }
                if (!TextUtils.isEmpty(styleHtml)) {
                    allStyleJson.put(jsonPathName, styleHtml);
                }
                Logger.d(TAG, " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                        + "\n 使用自定义的样式配置文件 : " + jsonFilePath);
            }
        }
        return styleHtml;
    }
}
