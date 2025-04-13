package com.fy.navi.service.logicpaket.layer;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.refix.LayerItemLabelResult;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.map.MapType;

/**
 * Author: QiuYaWei
 * Date: 2025/2/11
 * Description: [在这里描述文件功能]
 */
public interface ILayerPackageCallBack {

    default void onNotifyClick(MapType mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
    }


    default void onSearchItemClick(MapType mapTypeId, LayerItemSearchResult clickResult) {
    }

    default void onRouteItemClick(MapType mapTypeId, GemLayerItem pItem) {
    }

    default void onFavoriteClick(GeoPoint geoPoint) {

    }

    default void onFlyLineMoveEnd(MapType mapTypeId, GeoPoint descPoint) {

    }

    default void onCarClick(MapType mapType, GeoPoint geoPoint) {

    }

}
