package com.fy.navi.scene.impl.navi.search;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.ui.navi.search.NaviSearchResultListView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;

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
