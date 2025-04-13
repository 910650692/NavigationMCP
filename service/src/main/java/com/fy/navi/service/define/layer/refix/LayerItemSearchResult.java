package com.fy.navi.service.define.layer.refix;

import androidx.annotation.IntDef;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * 搜索结果bean类封装
 */
@Setter
@Getter
public class LayerItemSearchResult extends LayerItemBase {
    //此处type用作区分搜索大类  **必传字段
    private LayerSearchItemType type;
    //扎标类型  **必传字段
    private LayerSearchItemMarkerType itemMarkerType;
    //搜索结果封装数据 使用PoiInfoEntity.mPointTypeCode作扎标类型区分
    private ArrayList<PoiInfoEntity> searchResultPoints;
    //默认选中下标 list下标 从0开始
    private int selectedIndex = -1;
    //起点终点途经点
    private RouteLineLayerParam routeLineLayerParam;

    @Override
    public String toString() {
        return "LayerItemSearchResult{" +
                "type=" + type +
                ", itemMarkerType=" + itemMarkerType +
                ", searchResultPoints=" + searchResultPoints +
                ", selectedIndex=" + selectedIndex +
                ", routeLineLayerParam=" + routeLineLayerParam +
                '}';
    }
}
