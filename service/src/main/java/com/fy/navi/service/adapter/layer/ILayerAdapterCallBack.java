package com.fy.navi.service.adapter.layer;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.map.MapType;

/**
 * Author: QiuYaWei
 * Date: 2025/2/11
 * Description: [在这里描述文件功能]
 */
public interface ILayerAdapterCallBack {

    default void onNotifyClick(MapType mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {

    }

    default void onSearchItemClick(GemLayerItem pItem) {

    }

    default void onRouteItemClick(GemLayerItem pItem) {

    }

    default void onFavorite(double lat,double lon) {

    }

    default void onFlyLineMoveEnd(GeoPoint descPoint) {

    }

}
