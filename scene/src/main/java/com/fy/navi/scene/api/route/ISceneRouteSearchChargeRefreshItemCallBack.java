package com.fy.navi.scene.api.route;

import com.fy.navi.service.define.search.PoiInfoEntity;

public interface ISceneRouteSearchChargeRefreshItemCallBack {
    /**
     * 打开详情
     * @param poiInfoEntity   poi信息
     * */
    void enterToChargeDetails(PoiInfoEntity poiInfoEntity);
    /**
     * 移除
     * @param poiInfoEntity   poi信息
     * */
    void onGasChargeRemoveClick(PoiInfoEntity poiInfoEntity);
    /**
     * 添加
     * @param poiInfoEntity   poi信息
     * */

    void onGasChargeAddClick(PoiInfoEntity poiInfoEntity);
}
