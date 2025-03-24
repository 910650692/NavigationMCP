package com.fy.navi.service.adapter.search.bls;


import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.autonavi.gbl.search.model.AggregateSearchResult;
import com.autonavi.gbl.search.model.KeywordSearchResultV2;
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
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 统一管理搜索回调，支持动态注册与注销
 */
public final class SearchObserversHelper implements IKeyWordSearchObserverV2, ISuggestionSearchObserver,
        IPoiDetailSearchObserver, IPoiCmallDetailSearchObserver, ISceneSearchObserver, IPoiShopListSearchObserver,
        IGSearchNearestObserver, IGSearchLineDeepInfoObserver, IGSearchAlongWayObserver, IAggregateSearchObserver,
        ISearchEnrouteObserver , IGSearchDeepInfoObserver {

    private final Map<Class<?>, SearchCallbackWrapper<?>> mCallbackWrapperMap = new ConcurrentHashMap<>();
    private static final AtomicReference<SearchObserversHelper> INSTANCE = new AtomicReference<>();

    private SearchObserversHelper() {
    }

    public static SearchObserversHelper getInstance() {
        return INSTANCE.updateAndGet(prev -> prev != null ? prev : new SearchObserversHelper());
    }

    /**
     * 统一处理搜索结果
     *
     * @param taskId    taskId
     * @param errorCode errorCode 错误码
     * @param result    result 回调数据类
     * @param resultType resultType 回调数据类型
     * @param <T>       泛型参数
     */
    @SuppressWarnings("unchecked")
    private <T> void handleResult(final int taskId, final int errorCode, final T result, final Class<T> resultType) {
        Optional.ofNullable((SearchCallbackWrapper<T>) mCallbackWrapperMap.get(resultType))
                .ifPresent(callbackWrapper -> {
//                    try {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Received search result: "
                            + resultType.getSimpleName() + ", taskId=" + taskId + " ;errorCode: " + errorCode);
                    if (errorCode == Service.ErrorCodeOK) {
                        callbackWrapper.onSuccess(result);
                    } else {
                        callbackWrapper.onFailure(errorCode, result);
                    }
                    callbackWrapper.onComplete();
//                    } catch (Exception e) {
//                        Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Error handling result for " + resultType.getSimpleName(), e);
//                    }
                });
    }

    /**
     * 注册回调
     *
     * @param resultType resultType 注册回调类泛型
     * @param callbackWrapper callbackWrapper 回调监听
     * @param <T> 泛型参数
     */
    public <T> void registerCallback(final Class<T> resultType, final SearchCallbackWrapper<T> callbackWrapper) {
        mCallbackWrapperMap.put(resultType, callbackWrapper);
    }

    /**
     * 注销回调
     * @param resultType resultType 注销回调类泛型
     * @param <T> 泛型参数
     */
    public <T> void unregisterCallback(final Class<T> resultType) {
        mCallbackWrapperMap.remove(resultType);
    }

    /**
     * 清空所有回调
     */
    public void unInit() {
        mCallbackWrapperMap.clear();
    }

    // ========================== 各类搜索结果回调 ==========================

    @Override
    public void onGetKeyWordResult(final int taskId, final int errorCode, final KeywordSearchResultV2 pstResult) {
        handleResult(taskId, errorCode, pstResult, KeywordSearchResultV2.class);
    }

    @Override
    public void onGetSuggestionResult(final int taskId, final int errorCode, final SuggestionSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SuggestionSearchResult.class);
    }

    @Override
    public void onGetPoiDetailResult(final int taskId, final int errorCode, final PoiDetailSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, PoiDetailSearchResult.class);
    }

    @Override
    public void onGetPoiCmallDetailResult(final int taskId, final int errorCode, final PoiCmallDetailSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, PoiCmallDetailSearchResult.class);
    }

    @Override
    public void onGetSceneResult(final int taskId, final int errorCode, final SceneSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SceneSearchResult.class);
    }

    @Override
    public void onGetPoiShopListResult(final int taskId, final int errorCode, final PoiShopListSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, PoiShopListSearchResult.class);
    }

    @Override
    public void onGetNearestResult(final int taskId, final int errorCode, final SearchNearestResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SearchNearestResult.class);
    }

    @Override
    public void onGetLineDeepInfoResult(final int taskId, final int errorCode, final SearchLineDeepInfoResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SearchLineDeepInfoResult.class);
    }

    @Override
    public void onGetAlongWayResult(final int taskId, final int errorCode, final SearchAlongWayResult pstResult) {
        handleResult(taskId, errorCode, pstResult, SearchAlongWayResult.class);
    }

    @Override
    public void onGetAggregateResult(final int taskId, final int errorCode, final AggregateSearchResult pstResult) {
        handleResult(taskId, errorCode, pstResult, AggregateSearchResult.class);
    }

    @Override
    public void onResult(final SearchEnrouteResult searchEnrouteResult) {
        handleResult((int) searchEnrouteResult.taskId, searchEnrouteResult.errorCode, searchEnrouteResult, SearchEnrouteResult.class);
    }

    @Override
    public void onGetDeepInfoResult(final int taskId, final int errorCode, final SearchDeepInfoResult searchDeepInfoResult) {
        handleResult(taskId, errorCode, searchDeepInfoResult, SearchDeepInfoResult.class);
    }

    @NonNull
    @Override
    public String toString() {
        return "SearchObserversHelper{" +
                "callbackWrapperMap=" + mCallbackWrapperMap.keySet() +
                '}';
    }
}