package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteSearchRefreshList;
import com.fy.navi.scene.ui.route.SceneRouteSearchRefreshListView;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteSeachRefreshListImpl extends BaseSceneModel<SceneRouteSearchRefreshListView> implements ISceneRouteSearchRefreshList {
    private final RoutePackage mRoutePackage;
    public SceneRouteSeachRefreshListImpl(SceneRouteSearchRefreshListView mScreenView) {
        super(mScreenView);
        mRoutePackage = RoutePackage.getInstance();
    }
    @Override
    public void onItermAddClick(PoiInfoEntity poiInfoEntity) {
        if (!mRoutePackage.isBelongRouteParam(mMapTypeId, poiInfoEntity)) {
            mRoutePackage.addViaPoint(mMapTypeId, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
        }
    }
}
