package com.sgm.navi.scene.impl.search;


import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.search.ISceneTerminalParking;
import com.sgm.navi.scene.ui.search.SceneTerminalParkingListView;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.StackManager;

public class SceneTerminalViewImpl extends BaseSceneModel<SceneTerminalParkingListView> implements ISceneTerminalParking {
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    private int mTaskId;

    /**
     * 获取高德SDK请求任务Id
     * @return 请求任务Id
     */
    public int getMTaskId() {
        return mTaskId;
    }

    public SceneTerminalViewImpl(final SceneTerminalParkingListView sceneTerminalParking) {
        super(sceneTerminalParking);
        this.mSearchPackage = SearchPackage.getInstance();
        this.mRoutePackage = RoutePackage.getInstance();
    }

    @Override
    public void closeSearch() {
        BaseFragment baseFragment = StackManager.getInstance().getCurrentFragment(mMapTypeId.name());
        if (baseFragment != null) {
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        }
        mSearchPackage.clearTypeMark(LayerPointItemType.SEARCH_PARENT_PARK);
    }

    /**
     * 展示终点停车场扎标
     */
    public void showRoutePark(){
        mRoutePackage.showRoutePark(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 周边搜索
     * @param keyword 关键字
     * @param geoPoint 坐标
     */
    public void aroundSearch(final String keyword, final GeoPoint geoPoint) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, " aroundSearch: aroundSearch");
        mTaskId = mSearchPackage.aroundSearch(1, keyword, geoPoint, true);
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
        ThreadManager.getInstance().execute(() -> mRoutePackage.requestChangeEnd(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity));
    }

    /**
     * 设置终点停车场选中下标
     * @param poiInfoEntity 选中的实体类对象
     * @param index 选中下标
     */
    public void setSelectIndex(final PoiInfoEntity poiInfoEntity, final int index) {
        mSearchPackage.setSelectIndex(poiInfoEntity, index, AutoMapConstant.SearchType.TERMINAL_PARK_AROUND_SEARCH, true);
    }
}