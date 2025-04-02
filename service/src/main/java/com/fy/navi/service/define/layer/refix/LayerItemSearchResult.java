package com.fy.navi.service.define.layer.refix;

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
    //此处type仅区分搜索大类
    private int type;
    //如后续需要区分同一搜索结果返回的不同POI类型 需使用PoiInfoEntity.mPointTypeCode作区分
    private ArrayList<PoiInfoEntity> searchResultPoints;
    private int parentFocusIndex = -1;
}
