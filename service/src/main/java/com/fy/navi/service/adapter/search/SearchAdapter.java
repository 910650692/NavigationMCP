package com.fy.navi.service.adapter.search;

import android.util.Pair;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.SearchRequestParameter;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

/**
 * SearchAdapter类提供了一种方式来执行各种搜索操作和管理搜索回调.
 * 封装了对具体搜索API的调用.
 */
public class SearchAdapter {
    // 搜索API包名，用于动态加载实现类
    private static final String SEARCH_API_PKG = Objects.requireNonNull(SearchAdapter.class.getPackage()).getName();
    // 搜索API类名，用于动态加载实现类
    private static final String SEARCH_API_CLS = "SearchAdapterImpl";
    // ISearchApi接口的实现，用于执行搜索操作
    private final ISearchApi mSearchApi;

    /**
     * 私有构造方法，防止外部实例化.
     * 初始化mSearchApi，通过AdapterConfig动态加载指定的搜索API实现类.
     */
    private SearchAdapter() {
        mSearchApi = (ISearchApi) AdapterConfig.getObject(SEARCH_API_PKG, SEARCH_API_CLS);
    }

    /**
     * SearchAdapterHolder内部类用于实现单例模式的懒加载.
     */
    private static class SearchAdapterHolder {
        // 单例实例
        private static final SearchAdapter mInstance = new SearchAdapter();
    }

    /**
     * 获取SearchAdapter的单例实例.
     *
     * @return SearchAdapter的实例
     */
    public static SearchAdapter getInstance() {
        return SearchAdapterHolder.mInstance;
    }

    /**
     * 注册搜索结果回调.
     *
     * @param callback 搜索结果回调接口实现
     */
    public void registerCallBack(ISearchResultCallback callback) {
        mSearchApi.registerSearchObserver(callback);
    }

    /**
     * 反注册搜索结果回调.
     *
     * @param callback 搜索结果回调接口实现
     */
    public void unRegisterSearchObserver(ISearchResultCallback callback) {
        mSearchApi.unRegisterSearchObserver(callback);
    }

    /**
     * 初始化搜索API.
     */
    public void init() {
        mSearchApi.init();
    }

    public void unInit() {
        mSearchApi.unInitSearchApi();
    }

    /**
     * 执行预搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int suggestionSearch(SearchRequestParameter parameterBuilder) {
        return mSearchApi.suggestionSearch(parameterBuilder);
    }

    /**
     * 执行POI详情搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int poiDetailSearch(SearchRequestParameter parameterBuilder) {
        return mSearchApi.poiDetailSearch(parameterBuilder);
    }

    /**
     * 根据POI ID执行搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int poiIdSearch(SearchRequestParameter parameterBuilder) {
        return mSearchApi.poiIdSearch(parameterBuilder);
    }

    /**
     * 根据经纬度搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int geoSearch(SearchRequestParameter parameterBuilder) {
        return mSearchApi.geoSearch(parameterBuilder);
    }

    /**
     * 深度信息搜索.
     *
     * @param parameter 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int deppInfoSearch(SearchRequestParameter parameter) {
        return mSearchApi.deppInfoSearch(parameter);
    }

    /**
     * 沿途批量深度信息搜索
     *
     * @param requestParameter 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int doLineDeepInfoSearch(SearchRequestParameter requestParameter) {
        return mSearchApi.doLineDeepInfoSearch(requestParameter);
    }

    /**
     * 执行关键字搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int keywordSearch(SearchRequestParameter parameterBuilder) {
        return mSearchApi.keyWordSearch(parameterBuilder);
    }

    /**
     * 执行 沿途搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int alongWaySearch(SearchRequestParameter parameterBuilder) {
        return mSearchApi.alongWaySearch(parameterBuilder);
    }

    /**
     * 执行周边搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int aroundSearch(SearchRequestParameter parameterBuilder) {
        return mSearchApi.aroundSearch(parameterBuilder);
    }

    /**
     * V2聚合搜索
     *
     * @param searchRequestParameter 参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int aggregateSearch(SearchRequestParameter searchRequestParameter) {
        return mSearchApi.aggregateSearch(searchRequestParameter);
    }

    /**
     * V2顺路搜索
     *
     * @param searchRequestParameter 参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int enRouteKeywordSearch(SearchRequestParameter searchRequestParameter) {
        return mSearchApi.enRouteKeywordSearch(searchRequestParameter);
    }

    /**
     * 中止所有正在进行的搜索任务.
     */
    public void abortSearch() {
        mSearchApi.abortSearch();
    }

    /**
     * 中止指定的搜索任务.
     *
     * @param taskId taskID
     */
    public void abortSearch(int taskId) {
        mSearchApi.abortSearch(taskId);
    }

    /**
     * 获取预计到达时间
     *
     * @param searchRequestParameter SearchRequestParameter
     * @return distance ，travelTime
     */
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(SearchRequestParameter searchRequestParameter) {
        return mSearchApi.getTravelTimeFuture(searchRequestParameter);
    }

    /**
     * 获取预计到达时间,剩余距离,剩余电量
     *
     * @param searchRequestParameter SearchRequestParameter
     * @return ETAInfo
     */
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(SearchRequestParameter searchRequestParameter) {
        return mSearchApi.getTravelTimeFutureIncludeChargeLeft(searchRequestParameter);
    }
}
