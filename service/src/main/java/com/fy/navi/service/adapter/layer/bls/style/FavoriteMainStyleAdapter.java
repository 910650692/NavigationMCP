package com.fy.navi.service.adapter.layer.bls.style;

import com.autonavi.gbl.layer.FavoritePointLayerItem;
import com.autonavi.gbl.layer.model.BizUserType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.LayerItemType;
import com.autonavi.gbl.user.behavior.model.FavoriteType;
import com.fy.navi.service.adapter.layer.bls.impl.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.impl.ILayerItemProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemBase;

/**
 * @Description:
 * @author: chao.tang
 * @date: 2025年03月28日 21:49
 */

public class FavoriteMainStyleAdapter implements BaseStyleAdapter {

    private String favorite_main_poi_layer = "favorite_main_poi_layer.json";

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        int itemType = item.getItemType();
        int businessType = item.getBusinessType();
        switch (itemType) {
            case LayerItemType.LayerItemPointType:
                switch (businessType) {
                    case BizUserType.BizUserTypeFavoriteMain: {
                        FavoritePointLayerItem favoritePointLayerItem = (FavoritePointLayerItem)item;
                        if(favoritePointLayerItem.getMFavoriteType() == FavoriteType.FavoriteTypePoi){
                            return favorite_main_poi_layer;
                        }else if(favoritePointLayerItem.getMFavoriteType() == FavoriteType.FavoriteTypeHome){
                            return favorite_main_poi_layer;
                        }else if(favoritePointLayerItem.getMFavoriteType() == FavoriteType.FavoriteTypeCompany){
                            return favorite_main_poi_layer;
                        }
                    }
                }
        }
        return "";
    }

    @Override
    public LayerItemBase provideLayerItemDataProcessor(LayerItem item) {
        return null;
    }

    @Override
    public ILayerItemProcessor provideLayerItemProcessor(LayerItem item) {
        return null;
    }
}
