package com.fy.navi.scene.api.route;

import com.fy.navi.service.define.search.PoiInfoEntity;

public interface ISceneRouteSearchRefreshItemCallBack {
    /**
     * 进入详情
     * @param poiInfoEntity   poi信息
     * */
    void enterToDetails(PoiInfoEntity poiInfoEntity);
}
