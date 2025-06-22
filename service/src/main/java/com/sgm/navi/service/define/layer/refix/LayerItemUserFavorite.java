package com.sgm.navi.service.define.layer.refix;

import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * BizCruiseCongestionInfo
 */
@Setter
@Getter
public class LayerItemUserFavorite extends LayerItemBase {

    private ArrayList<PoiInfoEntity> mSimpleFavoriteList; //添加 poi 家 公司数据信息  根据不同type 区分

}
