package com.sgm.navi.scene.api.route;

import com.sgm.navi.service.define.search.PoiInfoEntity;

public interface ISceneRouteSearchRefreshList {
    /**
     * 添加途经点
     * @param poiInfoEntity   poi信息
     * */
    void onItermAddClick(PoiInfoEntity poiInfoEntity);

    /**
     * 删除途经点
     * @param poiInfoEntity   poi信息
     * */
    void onItermRemoveClick(PoiInfoEntity poiInfoEntity);
}
