package com.fy.navi.service.adapter.layer;

import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.map.MapTypeId;

/**
 * Author: QiuYaWei
 * Date: 2025/2/11
 * Description: [在这里描述文件功能]
 */
public interface ILayerAdapterCallBack {
    default void onBeforeNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {

    }

    ;

    default void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {

    }

    default void onAfterNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {

    }
}
