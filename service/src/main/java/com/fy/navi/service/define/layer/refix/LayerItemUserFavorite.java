package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * BizCruiseCongestionInfo
 */
@Setter
@Getter
public class LayerItemUserFavorite extends LayerItemBase {

    private ArrayList<PoiInfoEntity> mSimpleFavoriteList; //poi
    private PoiInfoEntity mHomeFavoriteList;   //家
    private PoiInfoEntity mCompanyFavoriteList; //公司

}
