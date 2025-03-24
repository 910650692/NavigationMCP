package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteResultList;
import com.fy.navi.scene.ui.route.SceneRouteResultListView;
import com.fy.navi.service.logicpaket.route.RoutePackage;

public class SceneRouteResultListImpl extends BaseSceneModel<SceneRouteResultListView> implements ISceneRouteResultList {

    private final RoutePackage mRoutePackage;

    public SceneRouteResultListImpl(final SceneRouteResultListView screenView) {
        super(screenView);
        mRoutePackage = RoutePackage.getInstance();
    }

    @Override
    public void selectRoute(final int index) {
        mRoutePackage.selectRoute(mMapTypeId, index);
    }
}
