package com.sgm.navi.scene.impl.route;

import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.route.ISceneRouteSearchRefreshList;
import com.sgm.navi.scene.ui.route.SceneRouteSearchRefreshListView;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

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

    /**
     * 删除途经点
     *
     * @param poiInfoEntity poi信息
     */
    @Override
    public void onItermRemoveClick(final PoiInfoEntity poiInfoEntity) {
        if (mRoutePackage.isBelongRouteParam(mMapTypeId, poiInfoEntity)) {
            mRoutePackage.removeVia(mMapTypeId, poiInfoEntity, true);
        }
    }
}
