package com.fy.navi.service.adapter.search.bls;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_SERVICE_TAG;

import androidx.annotation.NonNull;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.search.model.AggregateSearchResult;
import com.autonavi.gbl.search.model.KeywordSearchResultV2;
import com.autonavi.gbl.search.model.LineDeepQueryType;
import com.autonavi.gbl.search.model.LinePoiServiceAreaInfo;
import com.autonavi.gbl.search.model.PoiCmallDetailSearchResult;
import com.autonavi.gbl.search.model.PoiDetailSearchResult;
import com.autonavi.gbl.search.model.PoiShopListSearchResult;
import com.autonavi.gbl.search.model.SceneSearchResult;
import com.autonavi.gbl.search.model.SearchAlongWayResult;
import com.autonavi.gbl.search.model.SearchDeepInfoResult;
import com.autonavi.gbl.search.model.SearchEnrouteResult;
import com.autonavi.gbl.search.model.SearchLineDeepInfoResult;
import com.autonavi.gbl.search.model.SearchNearestResult;
import com.autonavi.gbl.search.model.SuggestionSearchResult;
import com.autonavi.gbl.search.observer.IAggregateSearchObserver;
import com.autonavi.gbl.search.observer.IGSearchAlongWayObserver;
import com.autonavi.gbl.search.observer.IGSearchDeepInfoObserver;
import com.autonavi.gbl.search.observer.IGSearchLineDeepInfoObserver;
import com.autonavi.gbl.search.observer.IGSearchNearestObserver;
import com.autonavi.gbl.search.observer.IKeyWordSearchObserverV2;
import com.autonavi.gbl.search.observer.IPoiCmallDetailSearchObserver;
import com.autonavi.gbl.search.observer.IPoiDetailSearchObserver;
import com.autonavi.gbl.search.observer.IPoiShopListSearchObserver;
import com.autonavi.gbl.search.observer.ISceneSearchObserver;
import com.autonavi.gbl.search.observer.ISearchEnrouteObserver;
import com.autonavi.gbl.search.observer.ISuggestionSearchObserver;
import com.autonavi.gbl.util.errorcode.common.Service;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

/**
 * 统一管理搜索回调，支持动态注册与注销
 */
public class SearchObserversHelper implements IKeyWordSearchObserverV2, ISuggestionSearchObserver,
        IPoiDetailSearchObserver, IPoiCmallDetailSearchObserver, ISceneSearchObserver, IPoiShopListSearchObserver,
        IGSearchNearestObserver, IGSearchLineDeepInfoObserver, IGSearchAlongWayObserver, IAggregateSearchObserver,
        ISearchEnrouteObserver , IGSearchDeepInfoObserver {

    private final Map<Class<?>, SearchCallbackWrapper<?>> callbackWrapperMap = new ConcurrentHashMap<>();
    private static final AtomicReference<SearchObserversHelper> INSTANCE = new AtomicReference<>();

    private SearchObserversHelper() {
    }

    public static SearchObserversHelper getInstance() {
        return INSTANCE.updateAndGet(prev -> prev != null ? prev : new SearchObserversHelper());
    }

    /**
     * 统一处理搜索结果
     */
    @SuppressWarnings("unchecked")
    private <T> void handleResult(int taskId, int errorCode, T result, Class<T> resultType) {
        Optional.ofNullable((SearchCallbackWrapper<T>) callbackWrapperMap.get(resultType))
                .ifPresent(callbackWrapper -> {
                    try {
                        Logger.d(SEARCH_SERVICE_TAG, "Received search result: " + resultType.getSimpleName() + ", taskId=" + taskId + " ;errorCode: " + errorCode
                                + " result: " + result.toString());
                        if (errorCode == Service.ErrorCodeOK) {
                            callbackWrapper.onSuccess(result);
                        } else {
                            callbackWrapper.onFailure(errorCode, result);
                        }
                        callbackWrapper.onComplete();
                    } catch (Exception e) {
                        Logger.e(SEARCH_SERVICE_TAG, "Error handling result for " + resultType.getSimpleName(), e);
                    }
                });
    }

    /**
     * 注册回调
     */
    public <T> void registerCallback(Class<T> resultType, SearchCallbackWrapper<T> callbackWrapper) {
        callbackWrapperMap.put(resultType, callbackWrapper);
    }

    /**
     * 注销回调
     */
    public <T> void unregisterCallback(Class<T> resultType) {
        callbackWrapperMap.remove(resultType);
    }

    /**
     * 清空所有回调
     */
    public void unInit() {
        callbackWrapperMap.clear();
    }

    // ========================== 各类搜索结果回调 ==========================

    @Override
    public void onGetKeyWordResult(int taskId, int errorCode, KeywordSearchResultV2 pstResult) {
        handleResult(taskId, errorCode, pstResult, KeywordSearchResultV2.class);
    }

    @Override
    public void onGetSuggestionResult(int taskId, int errorCode, SuggestionSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SuggestionSearchResult.class);
    }

    @Override
    public void onGetPoiDetailResult(int taskId, int errorCode, PoiDetailSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, PoiDetailSearchResult.class);
    }

    @Override
    public void onGetPoiCmallDetailResult(int taskId, int errorCode, PoiCmallDetailSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, PoiCmallDetailSearchResult.class);
    }

    @Override
    public void onGetSceneResult(int taskId, int errorCode, SceneSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SceneSearchResult.class);
    }

    @Override
    public void onGetPoiShopListResult(int taskId, int errorCode, PoiShopListSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, PoiShopListSearchResult.class);
    }

    @Override
    public void onGetNearestResult(int taskId, int errorCode, SearchNearestResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SearchNearestResult.class);
    }

    @Override
    public void onGetLineDeepInfoResult(int taskId, int errorCode, SearchLineDeepInfoResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SearchLineDeepInfoResult.class);
    }

    @Override
    public void onGetAlongWayResult(int taskId, int errorCode, SearchAlongWayResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SearchAlongWayResult.class);
    }

    @Override
    public void onGetAggregateResult(int taskId, int errorCode, AggregateSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, AggregateSearchResult.class);
    }

    @Override
    public void onResult(SearchEnrouteResult searchEnrouteResult) {
        handleResult((int) searchEnrouteResult.taskId, searchEnrouteResult.errorCode, searchEnrouteResult, SearchEnrouteResult.class);
    }

    @Override
    public void onGetDeepInfoResult(int taskId, int errorCode, SearchDeepInfoResult searchDeepInfoResult) {
        Logger.i("song---", GsonUtils.toJson(searchDeepInfoResult));
        handleResult(taskId, errorCode, searchDeepInfoResult, SearchDeepInfoResult.class);
    }

    @NonNull
    @Override
    public String toString() {
        return "SearchObserversHelper{" +
                "callbackWrapperMap=" + callbackWrapperMap.keySet() +
                '}';
    }
}