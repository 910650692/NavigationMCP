package com.sgm.navi.scene.impl.navi.search;

import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.ui.navi.search.NaviSearchResultListView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

public class NaviSearchResultListViewImpl extends BaseSceneModel<NaviSearchResultListView> {

    private final RoutePackage mRoutePackage;
    public NaviSearchResultListViewImpl(NaviSearchResultListView mScreenView) {
        super(mScreenView);
        mRoutePackage = RoutePackage.getInstance();
    }

    public void onItermAddClick(final PoiInfoEntity poiInfoEntity) {
        if (!mRoutePackage.isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
            mRoutePackage.addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
        }
    }
}
