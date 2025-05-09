package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.search.PoiInfoEntity;

import lombok.Getter;
import lombok.Setter;

/**
 * 终点可停车-自定义停车场扎标数据
 */
@Setter
@Getter
public class LayerItemSearchPark extends LayerItemData {

    private PoiInfoEntity parkInfo;

    @Override
    public String toString() {
        return "LayerItemSearchPark{" +
                "parkInfo=" + parkInfo +
                '}';
    }
}
