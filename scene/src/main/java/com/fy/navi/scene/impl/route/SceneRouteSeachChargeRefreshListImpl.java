package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteSearchRefreshList;
import com.fy.navi.scene.ui.route.SceneRouteSearchChargeRefreshListView;
import com.fy.navi.scene.ui.route.SceneRouteSearchRefreshListView;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteSeachChargeRefreshListImpl extends BaseSceneModel<SceneRouteSearchChargeRefreshListView> implements ISceneRouteSearchRefreshList {
    private final RoutePackage mRoutePackage;
    public SceneRouteSeachChargeRefreshListImpl(SceneRouteSearchChargeRefreshListView mScreenView) {
        super(mScreenView);
        mRoutePackage = RoutePackage.getInstance();
    }
    @Override
    public void onItermAddClick(PoiInfoEntity poiInfoEntity) {

    }
}
