package com.sgm.navi.service.adapter.layer.bls.texture;

import android.text.TextUtils;

import com.android.utils.file.ParseJsonUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.bls.style.BaseStyleAdapter;
import com.sgm.navi.service.define.map.MapType;

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

    private final ConcurrentHashMap<String, String> allStyleJson = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, String> allMarkerJson = new ConcurrentHashMap<>();

    public String getLayerStyleJson(MapType mapType, BaseLayer layer, LayerItem item, BaseStyleAdapter styleAdapter) {
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
                    Logger.e(TAG, mapType, " 未配置样式, 采用主屏 新建 自定义的样式配置文件  图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：", item.getItemType() + " 配置文件 :" + jsonFilePath);
                }
                if (!TextUtils.isEmpty(styleJson)) {
                    allStyleJson.put(jsonPathName, styleJson);
                    Logger.d(TAG, mapType, " 使用 新建 自定义的样式配置 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：", item.getItemType(), " 配置文件 :" + jsonFilePath);
                }
            } else {
                Logger.d(TAG, mapType, " 使用 缓存 自定义的样式配置 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：" + item.getItemType(), " 配置文件 ：" + jsonPathName);
            }
        }
        if (styleAdapter.isNeedRefreshStyleJson(item)) {
            styleJson = styleAdapter.refreshStyleJson(item, styleJson);
            Logger.e(TAG, mapType, " 重新 刷新 样式配置  图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：", item.getItemType());
        }
        return styleJson;
    }

    public TextureMarkerInfo getMarkerInfo(MapType mapType, BaseLayer layer, LayerItem item, String markerInfoName) {
        String markerInfoJson = null;
        if (!TextUtils.isEmpty(markerInfoName)) {
            if (markerInfoName.startsWith("{")) {
                markerInfoJson = markerInfoName;
            } else {
                markerInfoJson = allMarkerJson.get(markerInfoName);
                if (TextUtils.isEmpty(markerInfoJson)) {
                    String markerInfoFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
                            .append(mapType.getMapType())
                            .append(File.separator)
                            .append(markerInfoName).toString();
                    markerInfoJson = ParseJsonUtils.parseJsonFile(markerInfoFilePath);
                    if (TextUtils.isEmpty(markerInfoJson)) {
                        markerInfoJson = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
                                .append(MapType.MAIN_SCREEN_MAIN_MAP.getMapType())
                                .append(File.separator)
                                .append(markerInfoName).toString();
                        Logger.e(TAG, mapType, " 未配置markerInfo样式,采用主屏 新建 自定义的markerInfo  图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：" + item.getItemType(), "markerInfo文件:" + markerInfoFilePath);
                    }
                    if (!TextUtils.isEmpty(markerInfoJson)) {
                        allStyleJson.put(markerInfoName, markerInfoJson);
                        Logger.d(TAG, mapType, " 使用 新建 自定义的markerInfo  图层 :" + layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：", item.getItemType(), " markerInfo文件:" + markerInfoFilePath);
                    } else {
                        Logger.e(TAG, mapType, "使用 新建 自定义的markerInfo 出错   图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：", item.getItemType(), " markerInfo文件 :" + markerInfoFilePath);
                    }
                } else {
                    Logger.d(TAG, mapType, " 使用 缓存 自定义的markerInfo 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：" + item.getItemType(), " markerInfo文件：" + markerInfoName);
                }
            }
        }
        TextureMarkerInfo textureMarkerInfo = null;
        try {
            textureMarkerInfo = GsonUtils.fromJsonV2(markerInfoJson, TextureMarkerInfo.class);
        } catch (Exception exception) {
            Logger.e(TAG, mapType, "新建 默认的markerInfo 异常; 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：", item.getItemType(), "异常信息：" + exception.toString());
        } finally {
            if (textureMarkerInfo == null) {
                textureMarkerInfo = new TextureMarkerInfo();
                Logger.d(TAG, mapType, " 新建 默认的markerInfo 异常 ;使用 新建 默认的markerInfo ;图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：", item.getItemType());
            }
        }
        return textureMarkerInfo;
    }
}
