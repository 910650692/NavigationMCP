package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * 搜索结果bean类封装
 */
@Setter
@Getter
public class LayerItemSearchResult extends LayerItemBase {
    //搜索结果封装数据 使用PoiInfoEntity.mPointTypeCode作扎标类型区分
    private ArrayList<PoiInfoEntity> searchResultPoints;
}
