package com.fy.navi.service.adapter.search.bls;


import com.android.utils.log.Logger;
import com.autonavi.gbl.search.model.AggregateSearchResult;
import com.autonavi.gbl.search.model.KeywordSearchResultV2;
import com.autonavi.gbl.search.model.PoiDetailSearchResult;
import com.autonavi.gbl.search.model.SearchAlongWayResult;
import com.autonavi.gbl.search.model.SearchDeepInfoResult;
import com.autonavi.gbl.search.model.SearchEnrouteResult;
import com.autonavi.gbl.search.model.SearchLineDeepInfoResult;
import com.autonavi.gbl.search.model.SearchNearestResult;
import com.autonavi.gbl.search.model.SuggestionSearchResult;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.ISearchResultCallback;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.SearchRequestParameter;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.List;
import java.util.function.BiConsumer;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 */
public class SearchResultCallbackHelper {
    private final List<ISearchResultCallback> mSearchResponseCallbackList;

    public SearchResultCallbackHelper(final List<ISearchResultCallback> searchResponseCallbackList) {
        this.mSearchResponseCallbackList = searchResponseCallbackList;
    }

    /***
     * 搜索结果统一处理.
     * @param requestParameterBuilder SearchRequestParameterBuilder,HMI搜索参数，用于返回数据类型
     * @param result  搜索结果
     * @param taskId 任务ID
     * @param <T> GBL 搜索结构类型
     */
    public <T> void notifySearchSuccess(final int taskId, final SearchRequestParameter requestParameterBuilder, final T result) {
        if (requestParameterBuilder == null || result == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid parameters: requestParameterBuilder or result is null");
            return;
        }
        SearchResultEntity resultList = null;

        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "notifySearchSuccess: SearchType:"
                + requestParameterBuilder.getSearchType() + "  ;;taskId:" + taskId);

        switch (requestParameterBuilder.getSearchType()) {
            case AutoMapConstant.SearchType.SEARCH_KEYWORD:
            case AutoMapConstant.SearchType.AROUND_SEARCH:
            case AutoMapConstant.SearchType.TERMINAL_PARK_AROUND_SEARCH:
                resultList = handleKeywordSearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.SEARCH_SUGGESTION:
                resultList = handleSuggestionSearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.POI_SEARCH:
                resultList = handlePoiSearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.GEO_SEARCH:
                resultList = handleGeoSearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.ALONG_WAY_SEARCH:
                resultList = handleAlongWaySearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.AGGREGATE_SEARCH:
                resultList = handleAggregateSearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH:
                resultList = handleEnRouteKeywordSearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.DEEP_INFO_SEARCH:
                resultList = handleDeepInfoSearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH:
                resultList = handleLineDeepInfoSearch(requestParameterBuilder, result);
                break;
            case AutoMapConstant.SearchType.POI_DETAIL_SEARCH:
                resultList = handlePoiDetailSearch(requestParameterBuilder, result);
                break;
            default:
                Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Unknown search type: " + requestParameterBuilder.getSearchType());
                return;
        }
        notifyCallbacks(taskId, requestParameterBuilder, resultList);
    }

    /**
     * 关键字搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handleKeywordSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof KeywordSearchResultV2 keywordResult)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for keyword search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromKeywordSearchResultV2(requestParameterBuilder, keywordResult);
    }

    /**
     * 推荐搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handleSuggestionSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof SuggestionSearchResult suggestionResult)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for suggestion search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromSuggestionSearchResult(requestParameterBuilder, suggestionResult);
    }

    /**
     * POI搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handlePoiSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof KeywordSearchResultV2 poiResult)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for POI search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromPoiDetailsSearchResult(requestParameterBuilder, poiResult);
    }

    /**
     * 逆地理搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handleGeoSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof SearchNearestResult geoResult)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for geo search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromGeoSearchResult(requestParameterBuilder, geoResult);
    }

    /**
     * 深度搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handleDeepInfoSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof SearchDeepInfoResult searchDeepInfoResult)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for deep info search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromDeepInfoSearchResult(requestParameterBuilder, searchDeepInfoResult);
    }

    /**
     * 沿途批量深度搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handleLineDeepInfoSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof SearchLineDeepInfoResult lineDeepInfoResult)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for line deep info search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromLineDeepInfoSearchResult(requestParameterBuilder, lineDeepInfoResult);
    }

    /**
     * 顺路搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handleAlongWaySearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof SearchAlongWayResult alongWayResult)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for along-way search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromAlongWayResult(requestParameterBuilder, alongWayResult);
    }

    /**
     * 聚合搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handleAggregateSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof AggregateSearchResult aggregateSearchResult)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for aggregate search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromAggregateSearchResult(requestParameterBuilder, aggregateSearchResult);
    }

    /**
     * 顺路搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handleEnRouteKeywordSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof SearchEnrouteResult searchEnrouteResult)) {
          Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for en-route keyword search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromSearchEnRouteResult(requestParameterBuilder, searchEnrouteResult);
    }

    /**
     * POI详情搜索回调分发
     * @param requestParameterBuilder 请求参数
     * @param result 回调数据
     * @return SearchResultEntity
     * @param <T> 泛型参数
     */
    private <T> SearchResultEntity handlePoiDetailSearch(final SearchRequestParameter requestParameterBuilder, final T result) {
        if (!(result instanceof PoiDetailSearchResult poiDetailSearchResult)) {
         Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid result type for poi detail search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromSearchPoiDetailSearchResult(requestParameterBuilder, poiDetailSearchResult);
    }

    /**
     * 回调数据分发
     * @param taskId 任务ID
     * @param requestParameterBuilder 请求参数
     * @param resultList 回调数据
     */
    private void notifyCallbacks(final int taskId, final SearchRequestParameter requestParameterBuilder, final SearchResultEntity resultList) {
        if (mSearchResponseCallbackList == null || resultList == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Callbacks or resultList is null");
            return;
        }
        final int code = resultList.getCode();
        final String message = resultList.getMessage();
        for (ISearchResultCallback callback : mSearchResponseCallbackList) {
            if (requestParameterBuilder.isSilentSearch()) {
                callback.onSilentSearchResult(taskId, code, message, resultList);
            } else {
                callback.onSearchResult(taskId, code, message, resultList, requestParameterBuilder);
            }
        }
    }

    public void notifyNetCallbacks(final int taskId,BaseRep result){
        if (mSearchResponseCallbackList == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "net Callbacks is null");
            return;
        }
        for (ISearchResultCallback callback : mSearchResponseCallbackList) {
            callback.onNetSearchResult(taskId,result);
        }
    }

    /**
     * 通用方法：创建 SearchCallbackWrapper
     *
     * @param resultType 结果类型
     * @param onSuccess  成功回调
     * @param onFailure  失败回调
     * @param <T>        泛型参数
     * @return SearchCallbackWrapper
     */
    public <T> SearchCallbackWrapper<T> createCallbackWrapper(
            final Class<T> resultType,
            final BiConsumer<Integer, T> onSuccess,
            final BiConsumer<Integer, T> onFailure) {
        return new SearchCallbackWrapper<>(new IBLSearchCallback<T>() {
            @Override
            public void onSuccess(final int taskId, final T result) {
                onSuccess.accept(taskId, result);
            }

            @Override
            public void onFailure(final int errCode, final T data) {
                onFailure.accept(errCode, data);
            }

            @Override
            public void onComplete() {
            }
        });
    }

    /**
     * 销毁清除数据缓存
     */
    public void unInit() {
        if (mSearchResponseCallbackList != null) {
            mSearchResponseCallbackList.clear();
        }
    }
}
