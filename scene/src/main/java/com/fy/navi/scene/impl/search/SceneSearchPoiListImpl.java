package com.fy.navi.scene.impl.search;


import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneSearchPoiList;
import com.fy.navi.scene.ui.search.SceneSearchPoiList;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.List;

public class SceneSearchPoiListImpl extends BaseSceneModel<SceneSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage mSearchPackage;
    private final MapDataPackage mapDataPackage;
    private int mTaskId;

    public SceneSearchPoiListImpl(final SceneSearchPoiList scrollView) {
        super(scrollView);
        this.mSearchPackage = SearchPackage.getInstance();
        this.mapDataPackage = MapDataPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        mScreenView.clearEditText();
        mSearchPackage.clearLabelMark();
    }

    /**
     * 获取城市信息
     * @param acCode 城市编码
     * @return CityDataInfo
     */
    public CityDataInfo getCityInfo(final int acCode) {
        return mapDataPackage.getCityInfo(acCode);
    }

    /**
     * 获取城市编码
     * @return acCode
     */
    public int getAcCode() {
        return mSearchPackage.getAcCode();
    }

    /**
     * 关键字搜索
     * @param pageNum 页码
     * @param keyword 关键字
     */
    public void keywordSearch(final int pageNum, final String keyword) {
        logSearch("keywordSearch", keyword);
        mTaskId = mSearchPackage.keywordSearch(pageNum, keyword);
    }

    /**
     * 关键字搜索2.0
     *
     * @param keyword      关键字
     * @param pageNum      搜索页数
     * @param retain       筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传
     * @param classifyData 一筛参数
     * @param isSilentSearch 是否静默搜索
     */
    public void keywordSearch(final int pageNum, final String keyword, final String retain, final String classifyData, final boolean isSilentSearch) {
        logSearch("keywordSearch classifyData: ", classifyData);
        mTaskId = mSearchPackage.keywordSearch(pageNum, keyword, retain, classifyData, isSilentSearch);
    }

    /**
     * 周边搜索
     * @param pageNum 搜索页数
     * @param keyword   关键字
     * @param poiInfoEntity 周边搜索的中心点信息
     */
    public void aroundSearch(final int pageNum, final String keyword, final PoiInfoEntity poiInfoEntity) {
        if (poiInfoEntity == null || poiInfoEntity.getPoint() == null) {
            mTaskId = mSearchPackage.aroundSearch(pageNum, keyword);
            return;
        }
        final GeoPoint geoPoint = new GeoPoint(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat());
        mTaskId = mSearchPackage.aroundSearch(pageNum, keyword, geoPoint);
    }

    /**
     * 周边搜索
     * @param pageNum 搜索页数
     * @param keyword   关键字
     * @param poiInfoEntity 周边搜索的中心点信息
     * @param range 搜索范围
     */
    public void aroundSearch(final int pageNum, final String keyword, final PoiInfoEntity poiInfoEntity, final String range) {
        if (poiInfoEntity == null || poiInfoEntity.getPoint() == null) {
            logSearch("aroundSearch", "自车位置附件搜索");
            mTaskId = mSearchPackage.aroundSearch(pageNum, keyword);
            return;
        }
        final GeoPoint geoPoint = new GeoPoint(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat());
        logSearch("aroundSearch", keyword);
        mTaskId = mSearchPackage.aroundSearch(pageNum, keyword, geoPoint, range);
    }

    /**
     * 途经点搜索
     * @param keyword 关键字
     */
    public void alongWaySearch(final String keyword) {
        logSearch("alongWaySearch", keyword);
        mTaskId = mSearchPackage.enRouteKeywordSearch(keyword);
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
     * 打印日志
     * @param method 方法名
     * @param keyword 关键字
     */
    private void logSearch(final String method, final String keyword) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, method + " key: " + keyword);
    }

    /**
     * 是否是处于算路阶段
     * @return boolean
     */
    public boolean isAlongWaySearch() {
        return mSearchPackage.isAlongWaySearch();
    }

    /**
     * 获取POI类型编码
     * @param typeCode typeCode
     * @return POI类型编码
     */
    public int getPointTypeCode(final String typeCode) {
        return mSearchPackage.getPointTypeCode(typeCode);
    }

    /**
     * 添加poi标记
     * @param poiInfoEntities 搜索结果列表
     * @param index 选中下标
     */
    public void addPoiMarker(final List<PoiInfoEntity> poiInfoEntities, final int index) {
        mSearchPackage.createPoiMarker(poiInfoEntities, index);
    }
}