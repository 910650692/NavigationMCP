package com.fy.navi.scene.impl.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneTerminalParking;
import com.fy.navi.scene.ui.search.SceneTerminalParkingListView;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

public class SceneTerminalViewImpl extends BaseSceneModel<SceneTerminalParkingListView> implements ISceneTerminalParking {
    private final SearchPackage searchPackage;
    private final RoutePackage routePackage;
    private int taskId;

    public SceneTerminalViewImpl(SceneTerminalParkingListView sceneTerminalParking) {
        super(sceneTerminalParking);
        this.searchPackage = SearchPackage.getInstance();
        this.routePackage = RoutePackage.getInstance();
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        searchPackage.clearLabelMark();
    }

    public void aroundSearch(String keyword, GeoPoint geoPoint) {
        Logger.d(SEARCH_HMI_TAG, " aroundSearch: aroundSearch");
        taskId = searchPackage.aroundSearch(1, keyword, geoPoint);
    }

    public void abortSearch() {
        searchPackage.abortSearch();
    }

    public void abortSearch(int taskId) {
        searchPackage.abortSearch(taskId);
    }

    public void startRoute(PoiInfoEntity poiInfoEntity) {
        routePackage.requestChangeEnd(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
    }
}