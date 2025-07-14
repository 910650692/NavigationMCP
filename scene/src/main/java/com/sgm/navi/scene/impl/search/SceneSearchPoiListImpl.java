package com.sgm.navi.scene.impl.search;


import android.os.Bundle;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.search.ISceneSearchPoiList;
import com.sgm.navi.scene.ui.search.SceneSearchPoiList;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.user.usertrack.SearchHistoryItemBean;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.sgm.navi.ui.base.StackManager;

import java.util.HashSet;
import java.util.List;
import java.util.Set;


/**
 *
 */
public class SceneSearchPoiListImpl extends BaseSceneModel<SceneSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage mSearchPackage;
    private final MapDataPackage mapDataPackage;
    private final RoutePackage mRoutePackage;
    private final UserTrackPackage mUserTrackPackage;
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
        this.mUserTrackPackage = UserTrackPackage.getInstance();
        this.mCalibrationPackage = CalibrationPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        if (!ConvertUtils.isEmpty(StackManager.getInstance().getCurrentFragment(mMapTypeId.name()))) {
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        }
        mScreenView.clearEditText();
        mSearchPackage.clearLabelMark();
        //算路页面点击搜索加油站终点后返回，显示全揽画面
        OpenApiHelper.enterPreview(mMapTypeId);
    }

    @Override
    public void closeSearchOpenFromNavi() {
        if (!ConvertUtils.isEmpty(StackManager.getInstance().getCurrentFragment(mMapTypeId.name()))) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    final Bundle bundle = new Bundle();
                    bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
                    StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(bundle);
                }
            });
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
        mTaskId = mSearchPackage.keywordSearch(pageNum, isChargingGeneralSearchText(keyword), false);
        SearchHistoryItemBean item = new SearchHistoryItemBean();
        item.setName(keyword);
        item.setUpdateTime(System.currentTimeMillis());
        mUserTrackPackage.addSearchHistory(item);
    }

    /**
     * poi批量搜索
     * @param poiList pid list
     * @param scene 搜索场景
     */
    public void poiListSearch(final List<String> poiList, final int scene) {
        mSearchPackage.poiListSearch(poiList, scene, false);
    }

    /**
     * 关键字搜索
     * @param pageNum 页码
     * @param keyword 关键字
     * @param isReSearch 是否超时重算
     */
    public void keywordSearch(final int pageNum, final String keyword, final boolean isReSearch) {
        logSearch("keywordSearch", keyword);
        mTaskId = mSearchPackage.keywordSearch(pageNum, isChargingGeneralSearchText(keyword), false, isReSearch);
        final SearchHistoryItemBean item = new SearchHistoryItemBean();
        item.setName(keyword);
        item.setUpdateTime(System.currentTimeMillis());
        mUserTrackPackage.addSearchHistory(item);
    }

    /**
     * 关键字搜索
     * @param pageNum 页数
     * @param keyword 关键字
     * @param adCode 城市编码
     * @param isSilent 是否静默搜
     */
    public void keywordSearch(final int pageNum, final String keyword, final int adCode, final boolean isSilent, final boolean isReSearch) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "keywordSearch", keyword);
        mTaskId = mSearchPackage.keywordSearch(pageNum, isChargingGeneralSearchText(keyword), adCode, isSilent, isReSearch);
        final SearchHistoryItemBean item = new SearchHistoryItemBean();
        item.setName(keyword);
        item.setUpdateTime(System.currentTimeMillis());
        mUserTrackPackage.addSearchHistory(item);
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
        mTaskId = mSearchPackage.keywordSearch(pageNum, isChargingGeneralSearchText(keyword), retain, classifyData, isSilentSearch);
        SearchHistoryItemBean item = new SearchHistoryItemBean();
        item.setName(keyword);
        item.setUpdateTime(System.currentTimeMillis());
        mUserTrackPackage.addSearchHistory(item);
    }

    /**
     * 关键字搜索2.0(二筛版本)
     *
     * @param keyword      关键字
     * @param pageNum      搜索页数
     * @param retain       筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传
     * @param classifyData 二筛参数
     * @param isSilentSearch 是否静默搜索
     */
    public void keywordSearchByQuickFilter(final int pageNum, final String keyword, final String retain, final String classifyData, final boolean isSilentSearch) {
        logSearch("keywordSearch classifyData: ", classifyData);
        mTaskId = mSearchPackage.keywordSearchByQuickFilter(pageNum, isChargingGeneralSearchText(keyword), retain, classifyData, isSilentSearch);
        SearchHistoryItemBean item = new SearchHistoryItemBean();
        item.setName(keyword);
        item.setUpdateTime(System.currentTimeMillis());
        mUserTrackPackage.addSearchHistory(item);
    }

    /**
     * 周边筛选搜索2.0
     *
     * @param keyword      关键字
     * @param pageNum      搜索页数
     * @param retain       筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传
     * @param classifyData 一筛参数
     * @param isSilentSearch 是否静默搜索
     */
    public void aroundSearch(final int pageNum, final String keyword, final String retain,
                             final String classifyData, final boolean isSilentSearch, final PoiInfoEntity poiInfoEntity) {
        logSearch("keywordSearch classifyData: ", classifyData);
        if (poiInfoEntity == null || poiInfoEntity.getPoint() == null) {
            final GeoPoint userLoc = new GeoPoint();
            userLoc.setLon(PositionPackage.getInstance().getLastCarLocation().getLongitude());
            userLoc.setLat(PositionPackage.getInstance().getLastCarLocation().getLatitude());
            mTaskId = mSearchPackage.aroundSearch(pageNum, isChargingGeneralSearchText(keyword), retain, classifyData, isSilentSearch, userLoc);
            return;
        }
        final GeoPoint geoPoint = new GeoPoint(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat());
        mTaskId = mSearchPackage.aroundSearch(pageNum, isChargingGeneralSearchText(keyword), retain, classifyData, isSilentSearch, geoPoint);

    }

    /**
     * 周边搜索
     * @param pageNum 搜索页数
     * @param keyword   关键字
     * @param poiInfoEntity 周边搜索的中心点信息
     */
    public void aroundSearch(final int pageNum, final String keyword, final PoiInfoEntity poiInfoEntity) {
        if (poiInfoEntity == null || poiInfoEntity.getPoint() == null) {
            mTaskId = mSearchPackage.aroundSearch(pageNum, isChargingGeneralSearchText(keyword));
            return;
        }
        final GeoPoint geoPoint = new GeoPoint(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat());
        mTaskId = mSearchPackage.aroundSearch(pageNum, isChargingGeneralSearchText(keyword), geoPoint, false);
    }

    /**
     * 周边搜索
     * @param pageNum 搜索页数
     * @param keyword   关键字
     * @param poiInfoEntity 周边搜索的中心点信息
     * @param range 搜索范围
     * @param isReSearch 是否重搜
     */
    public void aroundSearch(final int pageNum, final String keyword, final PoiInfoEntity poiInfoEntity, final String range, final boolean isReSearch) {
        if (poiInfoEntity == null || poiInfoEntity.getPoint() == null) {
            logSearch("aroundSearch", "自车位置附件搜索");
            if (isReSearch) {
                final GeoPoint userLoc = new GeoPoint();
                userLoc.setLon(PositionPackage.getInstance().getLastCarLocation().getLongitude());
                userLoc.setLat(PositionPackage.getInstance().getLastCarLocation().getLatitude());
                mTaskId = mSearchPackage.reAroundSearch(pageNum, isChargingGeneralSearchText(keyword), userLoc, range);
            } else {
                mTaskId = mSearchPackage.aroundSearch(pageNum, isChargingGeneralSearchText(keyword));
            }
            return;
        }
        final GeoPoint geoPoint = new GeoPoint(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat());
        logSearch("aroundSearch", keyword);
        if (isReSearch) {
            mTaskId = mSearchPackage.reAroundSearch(pageNum, isChargingGeneralSearchText(keyword), geoPoint, range);
        } else {
            mTaskId = mSearchPackage.aroundSearch(pageNum, isChargingGeneralSearchText(keyword), geoPoint, range, false);
        }
    }

    /**
     * 途经点搜索
     * @param keyword 关键字
     */
    public void alongWaySearch(final String keyword) {
        logSearch("alongWaySearch", keyword);
        mTaskId = mSearchPackage.enRouteKeywordSearch(isChargingGeneralSearchText(keyword));
    }

    /**
     * 途经点搜索2.0
     * @param keyword 关键字
     */
    public void alongWaySearch(final String keyword, final String retain,
                               final String classifyData, final boolean isSilentSearch) {
        logSearch("alongWaySearch classifyData: ", classifyData);
        mTaskId = mSearchPackage.enRouteKeywordSearch(isChargingGeneralSearchText(keyword),retain,classifyData,isSilentSearch);
    }

    /**
     * 中止当前搜索
     */
    public void abortSearch() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "abortSearch");
        mTaskId = 0;
        mSearchPackage.abortSearch(getMTaskId());
    }

    /**
     * 中止当前搜索
     */
    public void abortSearchByTaskId() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "abortSearchByTaskId");
        mSearchPackage.abortSearch(getMTaskId());
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
    public void updatePoiMarker(final List<PoiInfoEntity> poiInfoEntities, final int index, final boolean isNeedPreview) {
        mSearchPackage.updatePoiMarker(poiInfoEntities, index, isNeedPreview);
    }

    /**
     * 设置扎标选中并高亮
     * @param poiInfoEntity 选中的实体类对象
     * @param index 选中的下标
     * @param searchType 扎标对应的搜索类型
     */
    public void setSelectIndex(final PoiInfoEntity poiInfoEntity, final int index, final int searchType) {
        mSearchPackage.setSelectIndex(poiInfoEntity, index, searchType, false);
    }

    /**
     * 沿途搜附加卡片点击事件
     * @param keyword 关键字
     * @param tabIndex 索引
     */
    public void onTabListGasChargeClick(final String keyword, final int tabIndex) {
        mListSearchType = tabIndex;
        if (tabIndex == 0) {
            mTaskId = mSearchPackage.enRouteKeywordSearch(isChargingGeneralSearchText(keyword));
        } else if (tabIndex == 1) {
            final RouteParam endPoint = mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
            if (endPoint != null && !ConvertUtils.isEmpty(endPoint.getRealPos())) {
                mTaskId = mSearchPackage.aroundSearch(1, isChargingGeneralSearchText(keyword), new GeoPoint(endPoint.getRealPos().getLon(), endPoint.getRealPos().getLat()), false);
            }
        } else {
            mTaskId = mSearchPackage.aroundSearch(1, isChargingGeneralSearchText(keyword));
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
    public void queryStationNewResult(SearchResultEntity searchResultEntity){
        if(ConvertUtils.isNull(searchResultEntity)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"searchResultEntity is null");
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"keyword: "+searchResultEntity.getKeyword());
        mTaskId = mSearchPackage.queryStationNewResult(searchResultEntity.getKeyword());
    }

    public int getBrand(){
        return mCalibrationPackage.brand();
    }


    /**
     * 转换泛搜关键字，根据UE需求
     * 搜索关键词包含以下要素时需要统一展示充电站结果（关键词显示保留用户输入值）：充电站、充电桩、快充、充电、补能
     * @param keyword 关键字
     * @return 转换后的关键字
     */
    private String isChargingGeneralSearchText(final String keyword) {
        if (!ConvertUtils.isEmpty(keyword)) {
            final Set<String> chargingGeneralText = new HashSet<>(Set.of(
                    "充电桩",
                    "快冲",
                    "充电",
                    "补能"
            ));
            if (chargingGeneralText.contains(keyword)) {
                return "充电站";
            } else {
                return keyword;
            }
        }
        return keyword;
    }
}