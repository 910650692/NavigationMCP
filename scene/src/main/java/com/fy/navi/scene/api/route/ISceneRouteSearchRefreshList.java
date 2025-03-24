package com.fy.navi.scene.api.route;

import com.fy.navi.service.define.search.PoiInfoEntity;

public interface ISceneRouteSearchRefreshList {
    /**
     * 添加途经点
     * @param poiInfoEntity   poi信息
     * */
    void onItermAddClick(PoiInfoEntity poiInfoEntity);
}
