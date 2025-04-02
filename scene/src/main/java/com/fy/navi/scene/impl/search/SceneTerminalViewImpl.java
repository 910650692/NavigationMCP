package com.fy.navi.scene.impl.search;


import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneTerminalParking;
import com.fy.navi.scene.ui.search.SceneTerminalParkingListView;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

public class SceneTerminalViewImpl extends BaseSceneModel<SceneTerminalParkingListView> implements ISceneTerminalParking {
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    private int mTaskId;

    public SceneTerminalViewImpl(final SceneTerminalParkingListView sceneTerminalParking) {
        super(sceneTerminalParking);
        this.mSearchPackage = SearchPackage.getInstance();
        this.mRoutePackage = RoutePackage.getInstance();
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        mSearchPackage.clearLabelMark();
    }

    /**
     * 周边搜索
     * @param keyword 关键字
     * @param geoPoint 坐标
     */
    public void aroundSearch(final String keyword, final GeoPoint geoPoint) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, " aroundSearch: aroundSearch");
        mTaskId = mSearchPackage.aroundSearch(1, keyword, geoPoint);
    }

    /**
     * 中止搜索
     */
    public void abortSearch() {
        mSearchPackage.abortSearch();
    }

    /**
     * 中止搜索
     * @param taskId 任务id
     */
    public void abortSearch(final int taskId) {
        mSearchPackage.abortSearch(taskId);
    }

    /**
     * 开始路线规划
     * @param poiInfoEntity poi信息实体
     */
    public void startRoute(final PoiInfoEntity poiInfoEntity) {
        mRoutePackage.requestChangeEnd(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
    }
}