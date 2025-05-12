package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.search.PoiInfoEntity;

import lombok.Getter;
import lombok.Setter;

/**
 * 搜索结果Poi-自定义扎标数据
 */
@Setter
@Getter
public class LayerItemSearchPoint extends LayerItemData {

    private PoiInfoEntity poiInfo;

    @Override
    public String toString() {
        return "LayerItemSearchPoint{" +
                "poiInfo=" + poiInfo +
                '}';
    }
}
