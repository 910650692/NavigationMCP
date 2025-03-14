package com.fy.navi.scene.impl.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneSearchPoiList;
import com.fy.navi.scene.ui.search.SceneSearchPoiList;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @Author: baipeng0904
 * @Description: 搜索POI列表的实现类
 */
public class SceneSearchPoiListImpl extends BaseSceneModel<SceneSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage searchPackage;
    private final MapDataPackage mapDataPackage;
    private int taskId;

    public SceneSearchPoiListImpl(SceneSearchPoiList scrollView) {
        super(scrollView);
        this.searchPackage = SearchPackage.getInstance();
        this.mapDataPackage = MapDataPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        mScreenView.clearEditText();
        searchPackage.clearLabelMark();
    }

    public CityDataInfo getCityInfo(int acCode) {
        return mapDataPackage.getCityInfo(acCode);
    }

    public int getAcCode() {
        return searchPackage.getAcCode();
    }

    public void keywordSearch(int pageNum, String keyword) {
        logSearch("keywordSearch", keyword);
        taskId = searchPackage.keywordSearch(pageNum, keyword);
    }

    public void keywordSearch(int pageNum, String keyword, String retain, String checkedLevel, String classifyData, boolean isSilentSearch) {
        logSearch("keywordSearch classifyData: ", classifyData);
        taskId = searchPackage.keywordSearch(pageNum, keyword, retain, checkedLevel, classifyData, isSilentSearch);
    }

    public void aroundSearch(int pageNum, String keyword, PoiInfoEntity poiInfoEntity) {
        if (poiInfoEntity == null || poiInfoEntity.getPoint() == null) {
            logSearch("aroundSearch", "自车位置附件搜索");
            taskId = searchPackage.aroundSearch(pageNum, keyword);
            return;
        }
        GeoPoint geoPoint = new GeoPoint(poiInfoEntity.getPoint().lon, poiInfoEntity.getPoint().lat);
        logSearch("aroundSearch", keyword);
        taskId = searchPackage.aroundSearch(pageNum, keyword, geoPoint);
    }

    public void alongWaySearch(String keyword) {
        logSearch("alongWaySearch", keyword);
        taskId = searchPackage.enRouteKeywordSearch(keyword);
    }

    public void abortSearch() {
        searchPackage.abortSearch();
    }

    public void abortSearch(int taskId) {
        searchPackage.abortSearch(taskId);
    }

    private void logSearch(String method, String keyword) {
        Logger.d(SEARCH_HMI_TAG, method + " key: " + keyword);
    }

    public boolean isAlongWaySearch() {
        return searchPackage.isAlongWaySearch();
    }

    public int getPointTypeCode(String typeCode) {
        return searchPackage.getPointTypeCode(typeCode);
    }
}