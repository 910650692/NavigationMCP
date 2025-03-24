package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteDetailsResultList;
import com.fy.navi.scene.ui.route.SceneRouteDetailsResultListView;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.logicpaket.route.RoutePackage;

public class SceneRouteDetailsResultListImpl extends BaseSceneModel<SceneRouteDetailsResultListView> implements ISceneRouteDetailsResultList {

    private final RoutePackage mRoutePackage;

    public SceneRouteDetailsResultListImpl(final SceneRouteDetailsResultListView screenView) {
        super(screenView);

        mRoutePackage = RoutePackage.getInstance();
    }
    /**
     * 避开道路
     * @param routeAvoidInfo 避开信息
     * */
    public void startAvoidRoad(final RouteAvoidInfo routeAvoidInfo) {
        mRoutePackage.setAvoidRoad(routeAvoidInfo);
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(mMapTypeId);
        param.setMRouteWay(RouteWayID.ROUTE_WAY_AVOID);
        mRoutePackage.requestRoute(param);
    }
}
