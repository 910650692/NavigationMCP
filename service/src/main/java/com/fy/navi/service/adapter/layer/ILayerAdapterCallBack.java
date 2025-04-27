package com.fy.navi.service.adapter.layer;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;

/**
 * Author: QiuYaWei
 * Date: 2025/2/11
 * Description: [在这里描述文件功能]
 */
public interface ILayerAdapterCallBack {

    default void onSearchItemClick(MapType mapTypeId, LayerPointItemType type, int index) {

    }

    default void onRouteItemClick(MapType mapTypeId, LayerPointItemType type, LayerItemRoutePointClickResult result) {

    }

    default void onFavoriteClick(MapType mapTypeId,PoiInfoEntity poiInfo) {

    }

    default void onFlyLineMoveEnd(MapType mapTypeId, GeoPoint descPoint) {

    }

    /**
     * 点击自车位
     * @param mapType
     * @param geoPoint
     */
    default void onCarClick(MapType mapType, GeoPoint geoPoint) {

    }

}
