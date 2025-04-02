package com.fy.navi.service.logicpaket.layer;

import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.refix.LayerItemCar;
import com.fy.navi.service.define.map.MapType;

/**
 * Author: QiuYaWei
 * Date: 2025/2/11
 * Description: [在这里描述文件功能]
 */
public interface ILayerPackageCallBack {
    default void onBeforeNotifyClick(MapType mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
    }

    default void onNotifyClick(MapType mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
    }

    default void onAfterNotifyClick(MapType mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
    }


    default void onCarClick(MapType mapTypeId, LayerItemCar layerItemCar) {
    }

    default void onFavorite(double lat,double lon) {

    }
}
