package com.fy.navi.scene.api.route;

import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public interface ISceneRouteSearchRefreshItemCallBack {

    void enterToDetails(PoiInfoEntity poiInfoEntity);
}
