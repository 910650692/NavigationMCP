package com.fy.navi.scene.impl.search;


import android.os.Bundle;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneSearchPoiList;
import com.fy.navi.scene.ui.search.SceneSearchPoiList;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.List;


/**
 *
 */
public class SceneSearchPoiListImpl extends BaseSceneModel<SceneSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage mSearchPackage;
    private final MapDataPackage mapDataPackage;
    private final RoutePackage mRoutePackage;
    private final CalibrationPackage mCalibrationPackage;
    private int mTaskId;
    private int mListSearchType;
    public int getMTaskId() {
        return mTaskId;
    }
    public SceneSearchPoiListImpl(final SceneSearchPoiList scrollView) {
        super(scrollView);
        this.mSearchPackage = SearchPackage.getInstance();
        this.mapDataPackage = MapDataPackage.getInstance();
        this.mRoutePackage = RoutePackage.getInstance();
        this.mCalibrationPackage = CalibrationPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        if (!ConvertUtils.isEmpty(StackManager.getInstance().getCurrentFragment(mMapTypeId.name()))) {
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        }
        mScreenView.clearEditText();
        mSearchPackage.clearLabelMark();
    }

    @Override
    public void closeSearchOpenFromNavi() {
        if (!ConvertUtils.isEmpty(StackManager.getInstance().getCurrentFragment(mMapTypeId.name()))) {
            final Bundle bundle = new Bundle();
            bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(bundle);
        }
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
        mTaskId = mSearchPackage.keywordSearch(pageNum, keyword, false);
    }

    /**
     * 关键字搜索
     * @param pageNum 页数
     * @param keyword 关键字
     * @param adCode 城市编码
     * @param isSilent 是否静默搜
     */
    public void keywordSearch(final int pageNum, final String keyword, final int adCode, final boolean isSilent) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "keywordSearch", keyword);
        mTaskId = mSearchPackage.keywordSearch(pageNum, keyword, adCode, isSilent);
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
        mTaskId = mSearchPackage.aroundSearch(pageNum, keyword, geoPoint, false);
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

    /**
     * 更新poi标记
     * @param poiInfoEntities 搜索结果列表
     * @param index 选中下标
     */
    public void updatePoiMarker(final List<PoiInfoEntity> poiInfoEntities, final int index) {
        mSearchPackage.updatePoiMarker(poiInfoEntities, index);
    }

    /**
     * 设置扎标选中并高亮
     * @param poiInfoEntity 选中的实体类对象
     * @param index 选中的下标
     * @param searchType 扎标对应的搜索类型
     */
    public void setSelectIndex(final PoiInfoEntity poiInfoEntity, final int index, final int searchType) {
        mSearchPackage.setSelectIndex(poiInfoEntity, index, searchType);
    }

    /**
     * 沿途搜附加卡片点击事件
     * @param keword 关键字
     * @param tabIndex 索引
     */
    public void onTabListGasChargeClick(final String keword, final int tabIndex) {
        mListSearchType = tabIndex;
        if (tabIndex == 0) {
            mTaskId = mSearchPackage.enRouteKeywordSearch(keword);
        } else if (tabIndex == 1) {
            final RouteParam endPoint = mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
            mTaskId = mSearchPackage.aroundSearch(1, keword, new GeoPoint(endPoint.getRealPos().getLon(), endPoint.getRealPos().getLat()), false);
        } else {
            mTaskId = mSearchPackage.aroundSearch(1, keword);
        }
    }

    /**
     * 沿途搜确认按钮点击事件
     * @param routeParams 批量添加途径点列表
     */
    public void onConfirm(final List<RouteParam> routeParams) {
        mRoutePackage.requestManyVia(MapType.MAIN_SCREEN_MAIN_MAP, routeParams);
    }

    /**
     * poiInfoEntity转换为RouteParam
     * @param poiInfoEntity poiInfoEntity
     * @return RouteParam
     */
    private RouteParam convertPoiInfoToRouteParam(final PoiInfoEntity poiInfoEntity) {
        return mRoutePackage.getRouteParamFromPoiInfoEntity(poiInfoEntity, 1);
    }


    /**
     * 清除搜索图层所有扎标
     */
    public void clearLabelMarker() {
        mSearchPackage.clearLabelMark();
    }

    // 动力类型
    public int powerType(){
        return mCalibrationPackage.powerType();
    }

    // 查询自营站列表
    public int queryStationNewResult(SearchResultEntity searchResultEntity){
        mTaskId = mSearchPackage.queryStationNewResult(searchResultEntity);
        return mTaskId;
    }

}