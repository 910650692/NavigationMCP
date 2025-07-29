package com.sgm.navi.scene.api.route;

import com.sgm.navi.service.define.search.PoiInfoEntity;

public interface ISceneRouteSearchRefreshItemCallBack {
    /**
     * 进入详情
     * @param poiInfoEntity   poi信息
     * @param index
     * */
    void enterToDetails(PoiInfoEntity poiInfoEntity, int index);
}
