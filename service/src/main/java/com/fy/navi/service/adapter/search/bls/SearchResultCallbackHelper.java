package com.fy.navi.service.adapter.search.bls;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_SERVICE_TAG;

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
import com.fy.navi.service.adapter.search.ISearchResultCallback;
import com.fy.navi.service.define.search.SearchRequestParameter;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class SearchResultCallbackHelper {
    private final List<ISearchResultCallback> mSearchResponseCallbackList;

    public SearchResultCallbackHelper(List<ISearchResultCallback> searchResponseCallbackList) {
        this.mSearchResponseCallbackList = searchResponseCallbackList;
    }

    /***
     * 搜索结果统一处理.
     * @param requestParameterBuilder SearchRequestParameterBuilder,HMI搜索参数，用于返回数据类型
     * @param result  搜索结果
     * @param <T> GBL 搜索结构类型
     */
    public <T> void notifySearchSuccess(int taskId, SearchRequestParameter requestParameterBuilder, T result) {
        if (requestParameterBuilder == null || result == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid parameters: requestParameterBuilder or result is null");
            return;
        }
        SearchResultEntity resultList = null;

        Logger.d(SEARCH_SERVICE_TAG, "notifySearchSuccess: SearchType:" + requestParameterBuilder.getSearchType() + "  ;;taskId:" + taskId);

        switch (requestParameterBuilder.getSearchType()) {
            case AutoMapConstant.SearchType.SEARCH_KEYWORD:
            case AutoMapConstant.SearchType.AROUND_SEARCH:
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
                Logger.e(SEARCH_SERVICE_TAG, "Unknown search type: " + requestParameterBuilder.getSearchType());
                return;
        }
        notifyCallbacks(taskId, requestParameterBuilder, resultList);
    }

    private <T> SearchResultEntity handleKeywordSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof KeywordSearchResultV2 keywordResult)) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid result type for keyword search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromKeywordSearchResultV2(requestParameterBuilder, keywordResult);
    }

    private <T> SearchResultEntity handleSuggestionSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof SuggestionSearchResult suggestionResult)) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid result type for suggestion search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromSuggestionSearchResult(requestParameterBuilder, suggestionResult);
    }

    private <T> SearchResultEntity handlePoiSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof KeywordSearchResultV2 poiResult)) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid result type for POI search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromPoiDetailsSearchResult(requestParameterBuilder, poiResult);
    }

    private <T> SearchResultEntity handleGeoSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof SearchNearestResult geoResult)) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid result type for geo search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromGeoSearchResult(requestParameterBuilder, geoResult);
    }

    private <T> SearchResultEntity handleDeepInfoSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof SearchDeepInfoResult searchDeepInfoResult)) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid result type for deep info search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromDeepInfoSearchResult(requestParameterBuilder, searchDeepInfoResult);
    }

    private <T> SearchResultEntity handleLineDeepInfoSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof SearchLineDeepInfoResult lineDeepInfoResult)) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid result type for line deep info search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromLineDeepInfoSearchResult(requestParameterBuilder, lineDeepInfoResult);
    }

    private <T> SearchResultEntity handleAlongWaySearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof SearchAlongWayResult alongWayResult)) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid result type for along-way search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromAlongWayResult(requestParameterBuilder, alongWayResult);
    }

    private <T> SearchResultEntity handleAggregateSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof AggregateSearchResult aggregateSearchResult)) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid result type for aggregate search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromAggregateSearchResult(requestParameterBuilder, aggregateSearchResult);
    }

    private <T> SearchResultEntity handleEnRouteKeywordSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof SearchEnrouteResult searchEnrouteResult)) {
          Logger.d(SEARCH_SERVICE_TAG, "Invalid result type for en-route keyword search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromSearchEnRouteResult(requestParameterBuilder, searchEnrouteResult);
    }

    private <T> SearchResultEntity handlePoiDetailSearch(SearchRequestParameter requestParameterBuilder, T result) {
        if (!(result instanceof PoiDetailSearchResult poiDetailSearchResult)) {
         Logger.d(SEARCH_SERVICE_TAG, "Invalid result type for poi detail search");
            return null;
        }
        return SearchResultMapper.getInstance().mapFromSearchPoiDetailSearchResult(requestParameterBuilder, poiDetailSearchResult);
    }

    private void notifyCallbacks(int taskId, SearchRequestParameter requestParameterBuilder, SearchResultEntity resultList) {
        if (mSearchResponseCallbackList == null || resultList == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Callbacks or resultList is null");
            return;
        }
        int code = resultList.getCode();
        String message = resultList.getMessage();
        for (ISearchResultCallback callback : mSearchResponseCallbackList) {
            if (requestParameterBuilder.isSilentSearch()) {
                callback.onSilentSearchResult(taskId, code, message, resultList);
            } else {
                callback.onSearchResult(taskId, code, message, resultList);
            }
        }
    }

    /**
     * 通用方法：创建 SearchCallbackWrapper
     */
    public <T> SearchCallbackWrapper<T> createCallbackWrapper(
            Class<T> resultType,
            Consumer<T> onSuccess,
            BiConsumer<Integer, T> onFailure) {
        return new SearchCallbackWrapper<>(new IBLSearchCallback<T>() {
            @Override
            public void onSuccess(T result) {
                onSuccess.accept(result);
            }

            @Override
            public void onFailure(int errCode, T data) {
                onFailure.accept(errCode, data);
            }

            @Override
            public void onComplete() {
            }
        });
    }

    public void unInit() {
        if (mSearchResponseCallbackList != null) {
            mSearchResponseCallbackList.clear();
        }
    }
}
