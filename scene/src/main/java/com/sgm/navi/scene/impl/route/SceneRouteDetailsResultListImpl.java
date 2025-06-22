package com.sgm.navi.scene.impl.route;

import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.route.ISceneRouteDetailsResultList;
import com.sgm.navi.scene.ui.route.SceneRouteDetailsResultListView;
import com.sgm.navi.service.define.route.RouteAvoidInfo;
import com.sgm.navi.service.define.route.RoutePriorityType;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RouteWayID;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

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
        ThreadManager.getInstance().execute(new Runnable() {
            @Override
            public void run() {
                mRoutePackage.setAvoidRoad(routeAvoidInfo);
                final RouteRequestParam param = new RouteRequestParam();
                param.setMMapTypeId(mMapTypeId);
                param.setMRouteWay(RouteWayID.ROUTE_WAY_AVOID);
                param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_STRATEGE);
                mRoutePackage.requestRoute(param);
            }
        });
    }
}
