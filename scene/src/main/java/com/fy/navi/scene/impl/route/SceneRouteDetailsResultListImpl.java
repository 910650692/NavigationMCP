package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteDetailsResultList;
import com.fy.navi.scene.ui.route.SceneRouteDetailsResultListView;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.logicpaket.route.RoutePackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteDetailsResultListImpl extends BaseSceneModel<SceneRouteDetailsResultListView> implements ISceneRouteDetailsResultList {

    private final RoutePackage mRoutePackage;

    public SceneRouteDetailsResultListImpl(SceneRouteDetailsResultListView mScreenView) {
        super(mScreenView);

        mRoutePackage = RoutePackage.getInstance();
    }

    public void startAvoidRoad(RouteAvoidInfo routeAvoidInfo) {
        mRoutePackage.setAvoidRoad(routeAvoidInfo);
        mRoutePackage.requestRoute(mMapTypeId, null, -1, false, RouteWayID.ROUTE_WAY_AVOID);
    }
}
