package com.fy.navi.service.adapter.search;

import android.util.Pair;

import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.SearchRequestParameter;

import java.util.concurrent.CompletableFuture;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public interface ISearchApi {
    // 初始化
    void init();

    void unInitSearchApi();

    // 注册搜索结果回调
    void registerSearchObserver(ISearchResultCallback ISearchResultCallback);

    // 取消注册搜索结果回调
    void unRegisterSearchObserver(ISearchResultCallback ISearchResultCallback);

    // V2 预搜索
    int suggestionSearch(SearchRequestParameter searchRequestParameterBuilder);

    // V2 关键字搜索
    int keyWordSearch(SearchRequestParameter searchRequestParameterBuilder);

    // V2聚合搜索
    int aggregateSearch(SearchRequestParameter searchRequestParameter);

    // V2顺路搜索
    int enRouteKeywordSearch(SearchRequestParameter searchRequestParameter);

    // V2 POI详情搜索
    int poiDetailSearch(SearchRequestParameter searchRequestParameterBuilder);

    // V2 POI ID搜索
    int poiIdSearch(SearchRequestParameter searchRequestParameterBuilder);

    // V1 度深度搜索
    int geoSearch(SearchRequestParameter searchRequestParameterBuilder);

    // V2 深度信息搜索
    int deppInfoSearch(SearchRequestParameter parameter);

    // V2 沿途批量搜索
    int doLineDeepInfoSearch(SearchRequestParameter parameter);

    // V1 沿途搜索
    int alongWaySearch(SearchRequestParameter searchRequestParameterBuilder);

    // V2 周边搜索
    int aroundSearch(SearchRequestParameter searchRequestParameterBuilder);

    int queryStationNewResult(SearchRequestParameter searchRequestParameter);

    // 取消所有搜索
    void abortSearch();

    // 取消单个搜索
    void abortSearch(int taskId);

    // 详情获取预计到达时间
    CompletableFuture<Pair<String, String>> getTravelTimeFuture(SearchRequestParameter searchRequestParameterBuilder);
    //获取预计到达时间，剩余距离，剩余点亮
    CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(SearchRequestParameter searchRequestParameterBuilder);

}
