package com.fy.navi.service.adapter.search;

import android.util.Pair;

import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.SearchRequestParameter;

import java.util.concurrent.CompletableFuture;

/**
 * @version \$Revision1.0\$
 * @author baipeng0904
 */
public interface ISearchApi {
    /**
     * 初始化
     */
    void init();

    /**
     * 销毁
     */
    void unInitSearchApi();

    /**
     * 注册搜索结果回调
     * @param isearchResultCallback ISearchResultCallback
     */
    void registerSearchObserver(ISearchResultCallback isearchResultCallback);

    /**
     * 取消注册搜索结果回调
     * @param isearchResultCallback ISearchResultCallback
     */
    void unRegisterSearchObserver(ISearchResultCallback isearchResultCallback);

    /**
     * V2 预搜索
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return taskId
     */
    int suggestionSearch(SearchRequestParameter searchRequestParameterBuilder);

    /**
     * V2 关键字搜索
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return taskId
     */
    int keyWordSearch(SearchRequestParameter searchRequestParameterBuilder);

    /**
     * V2聚合搜索
     * @param searchRequestParameter SearchRequestParameter
     * @return taskId
     */
    int aggregateSearch(SearchRequestParameter searchRequestParameter);

    /**
     * V2顺路搜索
     * @param searchRequestParameter SearchRequestParameter
     * @return taskId
     */
    int enRouteKeywordSearch(SearchRequestParameter searchRequestParameter);

    /**
     * V2 POI详情搜索
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return taskId
     */
    int poiDetailSearch(SearchRequestParameter searchRequestParameterBuilder);

    /**
     * V2 POI ID搜索
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return taskId
     */
    int poiIdSearch(SearchRequestParameter searchRequestParameterBuilder);

    /**
     * V1 度深度搜索
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return taskId
     */
    int geoSearch(SearchRequestParameter searchRequestParameterBuilder);

    /**
     * V2 深度信息搜索
     * @param parameter SearchRequestParameter
     * @return taskId
     */
    int deppInfoSearch(SearchRequestParameter parameter);

    /**
     * V2 沿途批量搜索
     * @param parameter SearchRequestParameter
     * @return taskId
     */
    int doLineDeepInfoSearch(SearchRequestParameter parameter);

    /**
     * V1 沿途搜索
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return taskId
     */
    int alongWaySearch(SearchRequestParameter searchRequestParameterBuilder);

    /**
     * V2 周边搜索
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return taskId
     */
    int aroundSearch(SearchRequestParameter searchRequestParameterBuilder);

    /**
     * 云端查询充电站结果
     * @param searchRequestParameter SearchRequestParameter
     * @return taskId
     */
    int queryStationNewResult(SearchRequestParameter searchRequestParameter);

    // 云端查询充电站收藏列表
    int queryCollectStation(SearchRequestParameter searchRequestParameter);

    // 云端查询充电站信息
    int queryStationInfo(SearchRequestParameter searchRequestParameter);

    // 云端查询充电桩信息
    int queryEquipmentInfo(SearchRequestParameter searchRequestParameter);

    // 云端创建预约
    int createReservation(SearchRequestParameter searchRequestParameter);

    // 云端解开地锁
    int unGroundLock(SearchRequestParameter searchRequestParameter);

    // 云端更新收藏
    int updateCollectStatus(SearchRequestParameter searchRequestParameter);

    // 云端查询预约
    int queryReservation(SearchRequestParameter searchRequestParameter);
    /**
     * 取消所有搜索
     */
    void abortSearch();

    /**
     * 取消单个搜索
     * @param taskId taskId
     */
    void abortSearch(int taskId);

    /**
     * 详情获取预计到达时间
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return CompletableFuture
     */
    CompletableFuture<Pair<String, String>> getTravelTimeFuture(SearchRequestParameter searchRequestParameterBuilder);
    /**
     * 获取预计到达时间，剩余距离，剩余电量
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return CompletableFuture
     */
    CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(SearchRequestParameter searchRequestParameterBuilder);


}
