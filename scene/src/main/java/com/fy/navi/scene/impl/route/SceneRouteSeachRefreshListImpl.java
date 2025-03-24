package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteSearchRefreshList;
import com.fy.navi.scene.ui.route.SceneRouteSearchRefreshListView;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;

public class SceneRouteSeachRefreshListImpl extends BaseSceneModel<SceneRouteSearchRefreshListView> implements ISceneRouteSearchRefreshList {
    private final RoutePackage mRoutePackage;
    public SceneRouteSeachRefreshListImpl(final SceneRouteSearchRefreshListView screenView) {
        super(screenView);
        mRoutePackage = RoutePackage.getInstance();
    }
    @Override
    public void onItermAddClick(final PoiInfoEntity poiInfoEntity) {
        if (!mRoutePackage.isBelongRouteParam(mMapTypeId, poiInfoEntity)) {
            mRoutePackage.addViaPoint(mMapTypeId, poiInfoEntity);
        }
    }
}
