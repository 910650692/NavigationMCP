package com.fy.navi.scene.api.route;

import com.fy.navi.service.define.search.PoiInfoEntity;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public interface ISceneRouteSearchChargeRefreshItemCallBack {

    void enterToChargeDetails(PoiInfoEntity poiInfoEntity);
    void onGasChargeRemoveClick(PoiInfoEntity poiInfoEntity);

    void onGasChargeAddClick(PoiInfoEntity poiInfoEntity);
}
