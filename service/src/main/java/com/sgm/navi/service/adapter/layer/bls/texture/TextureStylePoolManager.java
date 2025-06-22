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
    private static final String HTML = ".xml";

    private final ConcurrentHashMap<String, String> allStyleJson = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, String> allMarkerJson = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, String> allStyleHtml = new ConcurrentHashMap<>();

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
                    Logger.e(TAG, mapType + " 未配置样式,  图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                            + " 采用主屏 新建 自定义的样式配置文件 :" + jsonFilePath);
                }
                if (!TextUtils.isEmpty(styleJson)) {
                    allStyleJson.put(jsonPathName, styleJson);
                    Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                            + " 使用 新建 自定义的样式配置文件 :" + jsonFilePath);
                }
            } else {
                Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                        + " 使用 缓存 自定义的样式配置文件 ：" + jsonPathName);
            }
        }
        if (styleAdapter.isNeedRefreshStyleJson(item)) {
            styleJson = styleAdapter.refreshStyleJson(item, styleJson);
            Logger.d(TAG, getClass().getSimpleName() + " mapType = " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                    + ";重新 刷新 样式配置 ");
        }
        return styleJson;
    }

//    public String getHtml(MapType mapType, String defaultHtmPathName, BaseLayer layer, LayerItem item, BaseStyleAdapter styleAdapter) {
//        String styleHtml = null;
//        String htmlPathName = styleAdapter.provideLayerItemStyleHtml(item);
//        if (!TextUtils.isEmpty(htmlPathName)) {
//            if (!htmlPathName.endsWith(HTML)) {
//                htmlPathName = htmlPathName + HTML;
//            }
//            styleHtml = allStyleHtml.get(htmlPathName);
//            if (TextUtils.isEmpty(styleHtml)) {
//                String htmlFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
//                        .append(mapType.getMapType())
//                        .append(File.separator)
//                        .append(htmlPathName).toString();
//                styleHtml = ParseJsonUtils.parseHtmlFile(htmlFilePath);
//                if (TextUtils.isEmpty(styleHtml)) {
//                    htmlFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
//                            .append(MapType.MAIN_SCREEN_MAIN_MAP.getMapType())
//                            .append(File.separator)
//                            .append(htmlPathName).toString();
//                    styleHtml = ParseJsonUtils.parseHtmlFile(htmlFilePath);
//                    Logger.e(TAG, mapType + " 未配置HTM样式,  图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
//                            + " 采用主屏 新建 自定义的HTM样式 :" + htmlFilePath);
//
//                }
//                if (!TextUtils.isEmpty(styleHtml)) {
//                    allStyleJson.put(htmlPathName, styleHtml);
//                    Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
//                            + " 使用 新建 自定义的HTM样式 :" + htmlFilePath);
//                }
//            } else {
//                Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
//                        + " 使用 缓存 自定义的HTM样式 ：" + htmlPathName);
//            }
//        } else {
//            styleHtml = allStyleJson.get(defaultHtmPathName);
//            if (TextUtils.isEmpty(styleHtml)) {
//                String htmlFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_LAYER_CARDS_PATH)
//                        .append(defaultHtmPathName).toString();
//                styleHtml = ParseJsonUtils.parseHtmlFile(htmlFilePath);
//                if (!TextUtils.isEmpty(styleHtml)) {
//                    allStyleJson.put(defaultHtmPathName, styleHtml);
//                    Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
//                            + " 使用 新建 默认的HTM样式 :" + htmlFilePath);
//                }
//            } else {
//                Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
//                        + " 使用 缓存 默认的HTM样式 ：" + defaultHtmPathName);
//            }
//        }
//        return styleHtml;
//    }

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
                        Logger.e(TAG, mapType + " 未配置样式,  图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                                + " 采用主屏 新建 自定义的markerInfo :" + markerInfoFilePath);
                    }
                    if (!TextUtils.isEmpty(markerInfoJson)) {
                        allStyleJson.put(markerInfoName, markerInfoJson);
                        Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                                + " 使用 新建 自定义的markerInfo :" + markerInfoFilePath);
                    } else {
                        Logger.e(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                                + " 使用 失败 自定义的markerInfo :" + markerInfoFilePath);
                    }
                } else {
                    Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                            + " 使用 缓存 自定义的markerInfo ：" + markerInfoName);
                }
            }
        }
        TextureMarkerInfo textureMarkerInfo = null;
        try {
            textureMarkerInfo = GsonUtils.fromJsonV2(markerInfoJson, TextureMarkerInfo.class);
        } catch (Exception exception) {
            Logger.e(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                    + "   新建 默认的markerInfo 异常:" + exception.toString());
        } finally {
            if (textureMarkerInfo == null) {
                textureMarkerInfo = new TextureMarkerInfo();
                Logger.d(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                        + " 使用 新建 默认的markerInfo :");
            }
        }
        return textureMarkerInfo;
    }
}
