package com.sgm.navi.service.adapter.search.bls;


import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryReqStartPoints;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryRequestParam;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.search.model.AggregateSearchResult;
import com.autonavi.gbl.search.model.KeywordSearchIdqParam;
import com.autonavi.gbl.search.model.KeywordSearchResultV2;
import com.autonavi.gbl.search.model.KeywordSearchRqbxyParam;
import com.autonavi.gbl.search.model.KeywordSearchTQueryParam;
import com.autonavi.gbl.search.model.PoiDetailSearchResult;
import com.autonavi.gbl.search.model.SearchAggregateParam;
import com.autonavi.gbl.search.model.SearchAlongWayParam;
import com.autonavi.gbl.search.model.SearchAlongWayResult;
import com.autonavi.gbl.search.model.SearchBatchPoiDetailParam;
import com.autonavi.gbl.search.model.SearchBatchPoiDetailResult;
import com.autonavi.gbl.search.model.SearchDeepInfoParam;
import com.autonavi.gbl.search.model.SearchDeepInfoResult;
import com.autonavi.gbl.search.model.SearchEnrouteKeywordParam;
import com.autonavi.gbl.search.model.SearchEnrouteResult;
import com.autonavi.gbl.search.model.SearchLineDeepInfoParam;
import com.autonavi.gbl.search.model.SearchLineDeepInfoResult;
import com.autonavi.gbl.search.model.SearchMode;
import com.autonavi.gbl.search.model.SearchNearestParam;
import com.autonavi.gbl.search.model.SearchNearestResult;
import com.autonavi.gbl.search.model.SearchPoiChildInfo;
import com.autonavi.gbl.search.model.SearchPoiDetailParam;
import com.autonavi.gbl.search.model.SearchResult;
import com.autonavi.gbl.search.model.SearchSuggestionParam;
import com.autonavi.gbl.search.model.SuggestionSearchResult;
import com.autonavi.gbl.search.observer.IKeyWordSearchObserverV2;
import com.autonavi.gbl.util.errorcode.common.Service;
import com.autonavi.gbl.util.model.TaskResult;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.search.ISearchApi;
import com.sgm.navi.service.adapter.search.ISearchResultCallback;
import com.sgm.navi.service.adapter.search.cloudByPatac.api.SearchRepository;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.adapter.search.cloudByPatac.req.StationReq;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.ChildInfo;
import com.sgm.navi.service.define.search.ETAInfo;
import com.sgm.navi.service.define.search.SearchRequestParameter;
import com.sgm.navi.service.define.utils.BevPowerCarUtils;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.patac.netlib.callback.NetDisposableObserver;
import com.patac.netlib.exception.ApiException;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.schedulers.Schedulers;


/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索适配器实现类，用于处理各种搜索请求并管理回调。
 * @CreateDate: $ $
 */
public class SearchAdapterImpl extends SearchServiceV2Manager implements ISearchApi {
    private final String mErrorLog = "Search operation failed due to exception:";

    // 任务ID计数器，确保每个搜索任务都有唯一的ID
    private final AtomicInteger mTaskId = new AtomicInteger(0);

    // 存储搜索结果回调接口的线程安全列表
    private final List<ISearchResultCallback> mSearchResponseCallbackList = new CopyOnWriteArrayList<>();

    // 搜索观察者类，用于管理搜索回调
    private SearchObserversHelper mSearchObserversHelper;

    // 搜索结果通知类，用于分发搜索结果给注册的回调
    private SearchResultCallbackHelper mSearchNotificationHelper;

    private BLAosService mBLAosService;
    private static final Long MAXTIMEOUT = 30L;
    private static final Long TIPTIME = 5L;

    /**
     * 初始化搜索服务。
     */
    @Override
    public void init() {
        mBLAosService = new BLAosService();
        initService();
        mSearchObserversHelper = SearchObserversHelper.getInstance();
        mSearchNotificationHelper = new SearchResultCallbackHelper(mSearchResponseCallbackList);
    }

    @Override
    public void unInitSearchApi() {
        mSearchObserversHelper.unInit();
        mSearchNotificationHelper.unInit();
        unInit();
    }

    /**
     * 注册一个搜索结果回调接口，确保不重复添加相同的回调。
     *
     * @param callback 搜索结果回调接口
     */
    @Override
    public void registerSearchObserver(final ISearchResultCallback callback) {
        if (callback != null && !mSearchResponseCallbackList.contains(callback)) {
            mSearchResponseCallbackList.add(callback);
        }
    }

    /**
     * 取消注册一个搜索结果回调接口。
     *
     * @param callback 搜索结果回调接口
     */
    @Override
    public void unRegisterSearchObserver(final ISearchResultCallback callback) {
        if (callback != null) {
            mSearchResponseCallbackList.remove(callback);
        }
    }

    /**
     * 执行建议搜索，并返回任务ID。
     *
     * @param requestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int suggestionSearch(final SearchRequestParameter requestParameterBuilder) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "suggestionSearch");
        final SearchCallbackWrapper<SuggestionSearchResult> callbackWrapper = createCallbackWrapper(
                SuggestionSearchResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, requestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), requestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(SuggestionSearchResult.class, callbackWrapper);
        final SearchSuggestionParam param = SearchRequestParamV2.getInstance().convertToSearchSuggestionParamV2(requestParameterBuilder);
        final SearchResult searchResult = getSearchServiceV2().search(param, mSearchObserversHelper);
        mTaskId.set(searchResult.taskId);
        return mTaskId.get();
    }

    /**
     * 执行关键字搜索，并返回任务ID。
     *
     * @param requestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int keyWordSearch(final SearchRequestParameter requestParameterBuilder) {
        final boolean isReSearch = requestParameterBuilder.isMIsReSearch();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "keyWordSearch" + isReSearch);
        final SearchCallbackWrapper<KeywordSearchResultV2> callbackWrapper = createCallbackWrapper(
                KeywordSearchResultV2.class,
                (taskId, result) -> notifySearchSuccess(taskId, requestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), requestParameterBuilder, result)
        );
        mSearchObserversHelper.registerCallback(KeywordSearchResultV2.class, callbackWrapper);
        final KeywordSearchTQueryParam param = SearchRequestParamV2.getInstance().convertToSearchKeywordParamV2(requestParameterBuilder);
        if (isReSearch) {
            getSearchServiceV2().keyWordSearchTQuery(param, mSearchObserversHelper,
                    SearchMode.SEARCH_MODE_OFFLINE_ONLY, mTaskId.incrementAndGet());
        } else {
            getSearchServiceV2().keyWordSearchTQuery(param, mSearchObserversHelper,
                    SearchMode.SEARCH_MODE_ONLINE_ADVANCED, mTaskId.incrementAndGet());
        }

        return mTaskId.get();
    }

    /**
     * 执行聚合搜索，并返回任务ID。
     *
     * @param searchRequestParameter 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int aggregateSearch(final SearchRequestParameter searchRequestParameter) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "aggregateSearch");
        final SearchCallbackWrapper<AggregateSearchResult> callbackWrapper = createCallbackWrapper(
                AggregateSearchResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, searchRequestParameter, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), searchRequestParameter, result)
        );
        mSearchObserversHelper.registerCallback(AggregateSearchResult.class, callbackWrapper);
        final SearchAggregateParam param = SearchRequestParamV2.getInstance().convertToSearchAggregateParamV2(searchRequestParameter);
        final SearchResult searchResult = getSearchServiceV2().search(param, mSearchObserversHelper);
        mTaskId.set(searchResult.taskId);
        return mTaskId.get();
    }

    /**
     * 执行顺路搜索，并返回任务ID。
     *
     * @param searchRequestParameter 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int enRouteKeywordSearch(final SearchRequestParameter searchRequestParameter) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "enRouteKeywordSearch");
        final PathInfo pathInfo = (PathInfo) searchRequestParameter.getPathInfo();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "enRouteKeywordSearch pathInfo: " + pathInfo);
        final SearchEnrouteKeywordParam param = SearchRequestParamV2.getInstance().convertToSearchEnRouteKeywordParamV2(searchRequestParameter);
        final TaskResult searchResult = getSearchServiceV2().search(pathInfo, param, mSearchObserversHelper);
        mTaskId.set((int) searchResult.taskId);
        final SearchCallbackWrapper<SearchEnrouteResult> callbackWrapper = createCallbackWrapper(
                SearchEnrouteResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, searchRequestParameter, result),
                (errCode, result) -> notifySearchSuccess((int) searchResult.taskId, searchRequestParameter, result)
        );
        mSearchObserversHelper.registerCallback(SearchEnrouteResult.class, callbackWrapper);
        return mTaskId.get();
    }

    @Override
    public int poiListSearch(final SearchRequestParameter searchRequestParameter) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "poiListSearch");
        final SearchCallbackWrapper<SearchBatchPoiDetailResult> callbackWrapper = createCallbackWrapper(
                SearchBatchPoiDetailResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, searchRequestParameter, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), searchRequestParameter, result)
        );

        mSearchObserversHelper.registerCallback(SearchBatchPoiDetailResult.class, callbackWrapper);
        final SearchBatchPoiDetailParam param = SearchRequestParamV2.getInstance().convertToSearchBatchParam(searchRequestParameter);
        final TaskResult searchResult = getSearchServiceV2().search(param, mSearchObserversHelper);
        mTaskId.set((int)searchResult.taskId);
        return mTaskId.get();
    }

    /**
     * 执行周边搜索，并返回任务ID。
     *
     * @param requestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int aroundSearch(final SearchRequestParameter requestParameterBuilder) {
        final boolean isReSearch = requestParameterBuilder.isMIsReSearch();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "aroundSearch: " + isReSearch);
        final SearchCallbackWrapper<KeywordSearchResultV2> callbackWrapper = createCallbackWrapper(
                KeywordSearchResultV2.class,
                (taskId, result) -> notifySearchSuccess(taskId, requestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), requestParameterBuilder, result)
        );
        mSearchObserversHelper.registerCallback(KeywordSearchResultV2.class, callbackWrapper);
        final KeywordSearchRqbxyParam param = SearchRequestParamV2.getInstance().convertToAroundSearchParam(requestParameterBuilder);
        if (isReSearch) {
            getSearchServiceV2().keyWordSearchRqbxy(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_OFFLINE_ONLY, mTaskId.incrementAndGet());
        } else {
            getSearchServiceV2().keyWordSearchRqbxy(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ADVANCED, mTaskId.incrementAndGet());
        }
        return mTaskId.get();
    }

    /**
     * 执行POI详情搜索，并返回任务ID。
     *
     * @param requestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int poiDetailSearch(final SearchRequestParameter requestParameterBuilder) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "poiDetailSearch");
        final SearchCallbackWrapper<PoiDetailSearchResult> callbackWrapper = createCallbackWrapper(
                PoiDetailSearchResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, requestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), requestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(PoiDetailSearchResult.class, callbackWrapper);
        final SearchPoiDetailParam param = SearchRequestParamV2.getInstance().convertToSearchPoiDetailParamV2(requestParameterBuilder);
        getSearchServiceV2().poiDetailSearch(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ONLY, mTaskId.incrementAndGet());
        return mTaskId.get();
    }

    /**
     * 执行POI ID搜索，并返回任务ID。
     *
     * @param searchRequestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int poiIdSearch(final SearchRequestParameter searchRequestParameterBuilder) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "poiIdSearch");
        final SearchCallbackWrapper<KeywordSearchResultV2> callbackWrapper = createCallbackWrapper(
                KeywordSearchResultV2.class,
                (taskId, result) -> notifySearchSuccess(taskId, searchRequestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), searchRequestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(KeywordSearchResultV2.class, callbackWrapper);
        final KeywordSearchIdqParam param = SearchRequestParamV2.getInstance().convertToKeywordSearchIdqParam(searchRequestParameterBuilder);
        getSearchServiceV2().keyWordSearchIdq(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ADVANCED, mTaskId.incrementAndGet());
        return mTaskId.get();
    }

    /**
     * 执行地理坐标搜索，并返回任务ID。
     *
     * @param searchRequestParameterBuilder 搜索参数{@link SearchRequestParameter}
     * @return taskId
     */
    @Override
    public int geoSearch(final SearchRequestParameter searchRequestParameterBuilder) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "geoSearch");
        final SearchCallbackWrapper<SearchNearestResult> callbackWrapper = createCallbackWrapper(
                SearchNearestResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, searchRequestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), searchRequestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(SearchNearestResult.class, callbackWrapper);

        final SearchNearestParam nearestParam = new SearchNearestParam();
        nearestParam.poi_loc.lon = searchRequestParameterBuilder.getPoiLoc().getLon();
        nearestParam.poi_loc.lat = searchRequestParameterBuilder.getPoiLoc().getLat();
        getSearchServiceV1().nearestSearch(nearestParam, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ADVANCED, mTaskId.incrementAndGet());
        return mTaskId.get();
    }

    /**
     * 深度信息搜索，并返回任务ID。
     *
     * @param parameter 搜索参数{@link SearchRequestParameter}
     * @return taskId
     */
    @Override
    public int deppInfoSearch(final SearchRequestParameter parameter) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "deppInfoSearch");
        final SearchCallbackWrapper<SearchDeepInfoResult> callbackWrapper = createCallbackWrapper(
                SearchDeepInfoResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, parameter, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), parameter, result)
        );

        mSearchObserversHelper.registerCallback(SearchDeepInfoResult.class, callbackWrapper);

        final SearchDeepInfoParam param = new SearchDeepInfoParam();
        param.poiid = parameter.getPoiId();
        param.poi_loc.lat = parameter.getPoiLoc().getLat();
        param.poi_loc.lon = parameter.getPoiLoc().getLon();
        getSearchServiceV1().deepInfoSearch(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ONLY, mTaskId.incrementAndGet());
        return mTaskId.get();
    }

    /**
     * 执行线路深度信息搜索，并返回任务ID。
     *
     * @param parameter 搜索参数{@link SearchRequestParameter}
     * @return taskId
     */
    @Override
    public int doLineDeepInfoSearch(final SearchRequestParameter parameter) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "deppInfoSearch");
        final SearchCallbackWrapper<SearchLineDeepInfoResult> callbackWrapper = createCallbackWrapper(
                SearchLineDeepInfoResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, parameter, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), parameter, result)
        );

        mSearchObserversHelper.registerCallback(SearchLineDeepInfoResult.class, callbackWrapper);

        final SearchLineDeepInfoParam searchLineDeepInfoParam = new SearchLineDeepInfoParam();
        searchLineDeepInfoParam.poiIds = (java.util.ArrayList<String>) parameter.getPoiIdList();
        searchLineDeepInfoParam.queryType = Integer.parseInt(parameter.getQueryType());
        getSearchServiceV1().lineDeepInfoSearch(searchLineDeepInfoParam, mSearchObserversHelper,
                SearchMode.SEARCH_MODE_ONLINE_ONLY, mTaskId.incrementAndGet());
        return mTaskId.get();
    }

    /**
     * 执行 沿途搜索。
     *
     * @param searchRequestParameterBuilder SearchRequestParameterBuilder{@link SearchRequestParameter}
     * @return taskId
     */
    @Override
    public int alongWaySearch(final SearchRequestParameter searchRequestParameterBuilder) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "alongWaySearch");
        final SearchCallbackWrapper<SearchAlongWayResult> callbackWrapper = createCallbackWrapper(
                SearchAlongWayResult.class,
                (taskId, result) -> notifySearchSuccess(taskId, searchRequestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(mTaskId.get(), searchRequestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(SearchAlongWayResult.class, callbackWrapper);
        final SearchAlongWayParam param = SearchRequestParamV2.getInstance().convertToAlongWaySearchIdqParam(searchRequestParameterBuilder);
        getSearchServiceV1().alongWaySearch(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ADVANCED, mTaskId.incrementAndGet());
        return mTaskId.get();
    }

    /**
     * 中止所有正在进行的搜索任务。
     */
    @Override
    public void abortSearch() {
        getSearchServiceV2().abortAll();
    }

    /**
     * 中止单个正在进行的搜索任务。
     */
    @Override
    public void abortSearch(final int taskId) {
        getSearchServiceV2().abort(taskId);
    }


    /**
     * 获取预计到达时间、剩余距离
     *
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return distance ，travelTime
     */
    @Override
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(final SearchRequestParameter searchRequestParameterBuilder) {

        // TODO 后面需要对接真实的能耗模型参数
        //final GNavigationEtaqueryRequestParam requestParam =
        // SearchRequestParamV2.getInstance().convertToGNavigationEtaqueryRequestParam(searchRequestParameterBuilder);
        final GNavigationEtaqueryRequestParam requestParam = new GNavigationEtaqueryRequestParam();
        requestParam.start.points.add(new GNavigationEtaqueryReqStartPoints(13, 2,
                searchRequestParameterBuilder.getUserLoc().getLon(), searchRequestParameterBuilder.getUserLoc().getLat()));
        requestParam.end.points.add(new GNavigationEtaqueryReqStartPoints(143, 2,
                searchRequestParameterBuilder.getPoiLoc().getLon(), searchRequestParameterBuilder.getPoiLoc().getLat()));

        final CompletableFuture<Pair<String, String>> future = new CompletableFuture<>();
        mBLAosService.sendReqNavigationEtaquery(requestParam, response -> {
            if (response.route_list != null && !response.route_list.isEmpty()
                    && response.route_list.get(0).path != null && !response.route_list.get(0).path.isEmpty()) {
                final String distance = formatDistanceArrayInternal(response.route_list.get(0).path.get(0).distance);
                final String travelTime = TimeUtils.switchHourAndMimuteFromSecond(AppCache.getInstance().getMContext(),
                        (int) response.route_list.get(0).path.get(0).travel_time);
                final int chargeLeft = response.route_list.get(0).path.get(0).charge_left;
                future.complete(new Pair<>(distance, travelTime));
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "distance:" + distance + " travelTime:" + travelTime
                        + " chargeLeft:" + chargeLeft);
            } else {
                future.completeExceptionally(new Exception("No valid route data found"));
            }
        });
        return future;
    }

    /**
     * 获取预计到达时间、剩余距离、到达剩余电量
     *
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return distance ，travelTime
     */
    @Override
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(
            final SearchRequestParameter searchRequestParameterBuilder) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "getTravelTimeFutureIncludeChargeLeft remainCharge: "
                + BevPowerCarUtils.getInstance().initlialHVBattenergy
                + "maxBattery: " + BevPowerCarUtils.getInstance().maxBattenergy
                + "batteryDistance" + BevPowerCarUtils.getInstance().batterToDistanceCarSignal);
        // TODO 后面需要对接真实的能耗模型参数
        final GNavigationEtaqueryRequestParam requestParam = SearchRequestParamV2.getInstance().
                convertToGNavigationEtaqueryRequestParam(searchRequestParameterBuilder);
//        final GNavigationEtaqueryRequestParam requestParam = new GNavigationEtaqueryRequestParam();
        requestParam.start.points.add(new GNavigationEtaqueryReqStartPoints(13,
                2, searchRequestParameterBuilder.getUserLoc().getLon(),
                searchRequestParameterBuilder.getUserLoc().getLat()));
        requestParam.end.points.add(new GNavigationEtaqueryReqStartPoints(143,
                2, searchRequestParameterBuilder.getPoiLoc().getLon(),
                searchRequestParameterBuilder.getPoiLoc().getLat()));

        final CompletableFuture<ETAInfo> future = new CompletableFuture<>();
        mBLAosService.sendReqNavigationEtaquery(requestParam, response -> {
            if (response.route_list != null && !response.route_list.isEmpty()
                    && response.route_list.get(0).path != null
                    && !response.route_list.get(0).path.isEmpty()) {
                //公式:（当前续航-导航里程）/总续航
                final float currentBattery = BevPowerCarUtils.getInstance().initlialHVBattenergy < 0
                        ? 0f : BevPowerCarUtils.getInstance().initlialHVBattenergy;
                final float maxBattery = BevPowerCarUtils.getInstance().maxBattenergy < 0
                        ? 90.0f : BevPowerCarUtils.getInstance().maxBattenergy;
                final double batteryDistance = BevPowerCarUtils.getInstance().batterToDistanceCarSignal > 0 ?
                        BevPowerCarUtils.getInstance().batterToDistanceCarSignal : BevPowerCarUtils.getInstance().batterToDistance;
                final float chargeLeft = (float) (((currentBattery
                                        * batteryDistance)
                                        - response.route_list.get(0).path.get(0).distance)
                                        / (maxBattery * BevPowerCarUtils.getInstance().batterToDistance));
                int chargeLeftPercent = (int) (chargeLeft * 100);
                chargeLeftPercent = Math.max(0, chargeLeftPercent);
                final ETAInfo etaInfo = new ETAInfo()
                        .setDistance(response.route_list.get(0).path.get(0).distance)
                        .setTravelTime(TimeUtils.switchHourAndMimuteFromSecond(AppCache.getInstance().getMContext(),
                                (int) response.route_list.get(0).path.get(0).travel_time))
                        .setTime((int) response.route_list.get(0).path.get(0).travel_time)
                        .setLeftCharge(chargeLeftPercent);
                future.complete(etaInfo);
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "distance:" + etaInfo.getDistance() + " travelTime:"
                        + etaInfo.getTravelTime() + " batteryDistance: " + batteryDistance
                        + " chargeLeft:" + chargeLeftPercent);
            } else {
                future.completeExceptionally(new Exception("No valid route data found"));
            }
        });
        return future;
    }

    /**
     * 格式化距离数组
     *
     * @param distance 原始距离数据
     * @return 格式化后的距离文本
     */
    private String formatDistanceArrayInternal(final int distance) {
        final String[] distanceArray = ConvertUtils.formatDistanceArray(AppCache.getInstance().getMContext(), distance);
        return distanceArray[0] + distanceArray[1];
    }

    /**
     * 通知所有注册的回调搜索成功，并传递搜索结果。
     *
     * @param requestParameterBuilder 搜索参数构建器
     * @param result                  搜索结果
     * @param <T>                     泛型类型
     * @param taskId                  任务ID
     */
    private <T> void notifySearchSuccess(final int taskId, final SearchRequestParameter requestParameterBuilder, final T result) {
        mSearchNotificationHelper.notifySearchSuccess(taskId, requestParameterBuilder, result);
    }

    private void notifyNetSearchSuccess(final int taskId,String searchKey,BaseRep result){
        mSearchNotificationHelper.notifyNetCallbacks(taskId,searchKey,result);
    }

    private void notifyNetSearchError(final int taskId,String searchKey,String message){
        mSearchNotificationHelper.notifyNetCallbacksError(taskId,searchKey,message);
    }

    private void notifyTipDialog(String status){
        mSearchNotificationHelper.notifyTipDialog(status);
    }

    /**
     * 创建一个回调包装器，用于处理搜索结果的成功和失败情况。
     *
     * @param resultType 搜索结果类型
     * @param onSuccess  成功回调
     * @param onFailure  失败回调
     * @param <T>        泛型类型
     * @return 搜索回调包装器
     */
    private <T> SearchCallbackWrapper<T> createCallbackWrapper(
            final Class<T> resultType,
            final BiConsumer<Integer, T> onSuccess,
            final BiConsumer<Integer, T> onFailure) {
        return mSearchNotificationHelper.createCallbackWrapper(resultType, onSuccess, onFailure);
    }

    /**
     * 云端充电桩搜索
     * @return task id
     */
    @Override
    public int queryStationNewResult(final SearchRequestParameter searchRequestParameter) {
        mTaskId.incrementAndGet();
        StationReq req = new StationReq("1.0")
            .setFrom(String.valueOf(searchRequestParameter.getPage()))
            .setSize(String.valueOf(searchRequestParameter.getSize()))
            .setAreaCode(String.valueOf(searchRequestParameter.getAdCode()))
            .setKeyWords(searchRequestParameter.getKeyword())
            .setLat(String.valueOf(searchRequestParameter.getPoiLoc().getLat()))
            .setLng(String.valueOf(searchRequestParameter.getPoiLoc().getLon()));
        final Observable<String> observable = SearchRepository.getInstance().queryStationNewResult(req);
        observable.subscribeOn(Schedulers.io())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe(new NetDisposableObserver<String>() {
                @Override
                public void onSuccess(String data) {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                    try{
                        BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                        notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_STATION_LIST, rep);
                    }catch (Exception e){
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_STATION_LIST, e.getMessage());
                    }
                }

                @Override
                public void onFailed(ApiException apiException) {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                    notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_STATION_LIST, apiException.getMessage());
                }
            });
        return mTaskId.get();
    }

    /**
     * 云端收藏夹搜索
     * @return task id
     */
    @Override
    public int queryCollectStation(SearchRequestParameter searchRequestParameter) {
        mTaskId.incrementAndGet();
        StationReq req = new StationReq("1.0",searchRequestParameter.getIdpUserId())
                .setPageSize(searchRequestParameter.getSize())
                .setPageNum(searchRequestParameter.getPage())
                .setVehicleBrand(searchRequestParameter.getVehicleBrand());
        req.setAccessToken(searchRequestParameter.getAccessToken());
        final Observable<String> observable = SearchRepository.getInstance().queryCollectStation(req);
        observable.subscribeOn(Schedulers.io())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe(new NetDisposableObserver<String>() {
                @Override
                public void onSuccess(String data) {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                    try{
                        BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                        notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_COLLECT_LIST, rep);
                    }catch (Exception e){
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_COLLECT_LIST, e.getMessage());
                    }
                }

                @Override
                public void onFailed(ApiException apiException) {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                    notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_COLLECT_LIST, apiException.getMessage());
                }
            });
        return mTaskId.get();
    }

    /**
     * 充电站详情查询
     * @param searchRequestParameter 查询参数
     * @return task id
     */
    @Override
    public int queryStationInfo(SearchRequestParameter searchRequestParameter) {
        mTaskId.incrementAndGet();
        StationReq req = new StationReq("1.0")
                .setLng(String.valueOf(searchRequestParameter.getPoiLoc().getLon()))
                .setLat(String.valueOf(searchRequestParameter.getPoiLoc().getLat()))
                .setStationId(searchRequestParameter.getStationId())
                .setOperatorId(searchRequestParameter.getOperatorId());
        final Observable<String> observable = SearchRepository.getInstance().queryStationInfo(req);
        observable.subscribeOn(Schedulers.io())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe(new NetDisposableObserver<String>() {
                @Override
                public void onSuccess(String data) {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                    try{
                        BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                        notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_STATION_INFO, rep);
                    }catch (Exception e){
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_STATION_INFO, e.getMessage());
                    }
                }

                @Override
                public void onFailed(ApiException apiException) {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                    notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_STATION_INFO, apiException.getMessage());
                }
            });
        return mTaskId.get();
    }

    /**
     * 充电桩详情查询
     * @param searchRequestParameter 查询参数
     * @return task id
     */
    @Override
    public int queryEquipmentInfo(SearchRequestParameter searchRequestParameter) {
        mTaskId.incrementAndGet();
        StationReq req = new StationReq("1.0")
                .setOperatorId(searchRequestParameter.getOperatorId())
                .setStationId(searchRequestParameter.getStationId())
                .setEquipmentId(searchRequestParameter.getEquipmentId());
        final Observable<String> observable = SearchRepository.getInstance().queryEquipmentInfo(req);
        observable.subscribeOn(Schedulers.io())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe(new NetDisposableObserver<String>() {
                @Override
                public void onSuccess(String data) {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                    try{
                        BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                        notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO, rep);
                    }catch (Exception e){
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO, e.getMessage());
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                    }
                }

                @Override
                public void onFailed(ApiException apiException) {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                    notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO, apiException.getMessage());
                }
            });
        return mTaskId.get();
    }

    /**
     * 创建预约
     * @param searchRequestParameter 请求参数
     * @return task id
     */
    @Override
    public int createReservation(SearchRequestParameter searchRequestParameter) {
        mTaskId.incrementAndGet();
        StationReq req = new StationReq("1.0",searchRequestParameter.getIdpUserId())
                .setOperatorId(searchRequestParameter.getOperatorId())
                .setStationId(searchRequestParameter.getStationId())
                .setBrandId(searchRequestParameter.getVehicleBrand())
                .setSource(searchRequestParameter.getSource())
                .setConnectorId(searchRequestParameter.getConnectorId());
        req.setAccessToken(searchRequestParameter.getAccessToken());
        final Observable<String> observable = SearchRepository.getInstance().createReservation(req);
        observable.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new NetDisposableObserver<String>() {
                    @Override
                    public void onSuccess(String data) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                        try{
                            BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                            notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.CREATE_RESERVATION, rep);
                        }catch (Exception e){
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                            notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.CREATE_RESERVATION, e.getMessage());
                        }
                    }

                    @Override
                    public void onFailed(ApiException apiException) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.CREATE_RESERVATION, apiException.getMessage());
                    }
                });

        return mTaskId.get();
    }

    /**
     * 解开地锁
     * @param searchRequestParameter 入参
     * @return taskId
     */
    public int unGroundLock(SearchRequestParameter searchRequestParameter) {
        mTaskId.incrementAndGet();
        StationReq req = new StationReq("1.0",searchRequestParameter.getIdpUserId())
                .setOperatorId(searchRequestParameter.getOperatorId())
                .setBrandId(searchRequestParameter.getVehicleBrand())
                .setSource(searchRequestParameter.getSource())
                .setConnectorId(searchRequestParameter.getConnectorId());
        req.setAccessToken(searchRequestParameter.getAccessToken());
        final Observable<String> observable = SearchRepository.getInstance().unLockStation(req);
        observable.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new NetDisposableObserver<String>() {
                    @Override
                    public void onSuccess(String data) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                        try{
                            BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                            notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.UNLOCK_GROUND, rep);
                        }catch (Exception e){
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                            notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.UNLOCK_GROUND, e.getMessage());
                        }
                    }

                    @Override
                    public void onFailed(ApiException apiException) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.UNLOCK_GROUND, apiException.getMessage());
                    }
                });

        return mTaskId.get();
    }

    /**
     * 更新收藏状态
     * @param searchRequestParameter 入参
     * @return taskId
     */
    public int updateCollectStatus(SearchRequestParameter searchRequestParameter){
        mTaskId.incrementAndGet();
        Gson gson = new GsonBuilder().disableHtmlEscaping().create();
        HashMap<String, Object> map = new HashMap();
        map.put("savedStations",searchRequestParameter.getSavedStationsJson());
        map.put("updateType","FULL");
        map.put("channel",searchRequestParameter.getChannel());
        map.put("vehicleBrand",searchRequestParameter.getVehicleBrand());
        String json = gson.toJson(map);
        StationReq req = new StationReq("1.0",searchRequestParameter.getIdpUserId());
        req.setAccessToken(searchRequestParameter.getAccessToken());
        final Observable<String> observable = SearchRepository.getInstance().updateCollectStation(req,json);
        observable.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new NetDisposableObserver<String>() {
                    @Override
                    public void onSuccess(String data) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                        try{
                            BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                            notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.UPDATE_COLLECT, rep);
                        }catch (Exception e){
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                            notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.UPDATE_COLLECT, e.getMessage());
                        }
                    }

                    @Override
                    public void onFailed(ApiException apiException) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.UPDATE_COLLECT, apiException.getMessage());
                    }
                });
        return mTaskId.get();
    }

    /**
     * 查询预约单
     * @param searchRequestParameter 入参
     * @return taskId
     */
    public int queryReservation(SearchRequestParameter searchRequestParameter){
        mTaskId.incrementAndGet();
        StationReq req = new StationReq("1.0",searchRequestParameter.getIdpUserId())
                .setStatus(ConvertUtils.isNull(searchRequestParameter.getType()) ? null : searchRequestParameter.getType())
                .setPreNum(ConvertUtils.isNull(searchRequestParameter.getPreNum()) ? null : searchRequestParameter.getPreNum())
                .setBrandId(searchRequestParameter.getVehicleBrand());
        req.setAccessToken(searchRequestParameter.getAccessToken());
        final Observable<String> observable = SearchRepository.getInstance().queryReservation(req);
        observable.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new NetDisposableObserver<String>() {
                    @Override
                    public void onSuccess(String data) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                        try{
                            BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                            notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_RESERVATION, rep);
                        }catch (Exception e){
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                            notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_RESERVATION, e.getMessage());
                        }
                    }

                    @Override
                    public void onFailed(ApiException apiException) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.QUERY_RESERVATION, apiException.getMessage());
                    }
                });

        return mTaskId.get();
    }

    /**
     * 取消预约
     * @param searchRequestParameter 入参
     * @return taskId
     */
    public int cancelReservation(SearchRequestParameter searchRequestParameter){
        mTaskId.incrementAndGet();
        StationReq req = new StationReq("1.0",searchRequestParameter.getIdpUserId())
                .setPreNum(searchRequestParameter.getPreNum())
                .setStatus(3);
        req.setAccessToken(searchRequestParameter.getAccessToken());
        final Observable<String> observable = SearchRepository.getInstance().updateReservation(req);
        observable.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new NetDisposableObserver<String>() {
                    @Override
                    public void onSuccess(String data) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"success");
                        try{
                            BaseRep rep = GsonUtils.fromJson(String.valueOf(data),BaseRep.class);
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"data: "+rep.getResultCode());
                            notifyNetSearchSuccess(mTaskId.get(), AutoMapConstant.NetSearchKey.UPDATE_RESERVATION, rep);
                        }catch (Exception e){
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Exce: "+e);
                            notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.UPDATE_RESERVATION, e.getMessage());
                        }
                    }

                    @Override
                    public void onFailed(ApiException apiException) {
                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"Exce: "+apiException);
                        notifyNetSearchError(mTaskId.get(), AutoMapConstant.NetSearchKey.UPDATE_RESERVATION, apiException.getMessage());
                    }
                });

        return mTaskId.get();
    }

    /**
     * 计算预约剩余时间
     * @param lastTime 预约时间
     * @param travelTime 到达时间
     */
    public void calcTip(Long lastTime, Long travelTime){
        Long currentTime = TimeUtils.getInstance().getCurrentMillSeconds();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"lastTime: "+lastTime);
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"currentTime: "+currentTime);
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"travelTime: "+travelTime);
        Long time = TimeUtils.getInstance().convert2Minutes(currentTime - lastTime, TimeUnit.MILLISECONDS);
        // 预约剩余时间-到达时间 < 5 min
        if(MAXTIMEOUT - time > 0 && (MAXTIMEOUT - time) - travelTime < TIPTIME){
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"timeQuick");
            notifyTipDialog("timeQuick");
            return;
        }
        // 超过最大预约时间
        if(time > MAXTIMEOUT){
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"timeOut");
            notifyTipDialog("timeOut");
        }
    }


    /**
     * 获取预计到达时间、剩余距离、到达剩余电量
     *
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return distance ，travelTime
     */
    @Override
    public CompletableFuture<ChildInfo> setGrandChildInfoList(final SearchRequestParameter searchRequestParameterBuilder, final ChildInfo childInfo) {
        final KeywordSearchIdqParam param = SearchRequestParamV2.getInstance().convertToKeywordSearchIdqParam(searchRequestParameterBuilder);
        final CompletableFuture<ChildInfo> future = new CompletableFuture<>();

        getSearchServiceV2().keyWordSearchIdq(param, new IKeyWordSearchObserverV2() {
            @Override
            public void onGetKeyWordResult(final int taskId, final int errorCode, final KeywordSearchResultV2 keywordSearchResultV2) {
                if (errorCode == Service.ErrorCodeOK) {
                    getGrandChildList(keywordSearchResultV2, childInfo);
                    future.complete(childInfo);
                } else {
                    future.completeExceptionally(new Exception("No valid child data found"));
                }
            }
        }, SearchMode.SEARCH_MODE_ONLINE_ADVANCED, mTaskId.incrementAndGet());

        return future;
    }

    /**
     * 给当前子点赋值孙节点列表数据
     * @param result 搜索结果回调
     * @param childInfo 当前子点
     */
    private void getGrandChildList(final KeywordSearchResultV2 result, final ChildInfo childInfo) {
        //子节点信息
        if (!ConvertUtils.isEmpty(result.poiList)) {
            final List<ChildInfo> grandChildInfos = Optional.ofNullable(result.poiList.get(0).childInfoList)
                    .orElse(new ArrayList<>())
                    .stream()
                    .map(this::mapSearchPoiChildInfo)
                    .collect(Collectors.toList());
            childInfo.setMGrandChildInfoList(grandChildInfos);
        }
    }

    /**
     * 映射 SearchPoiChildInfo 到 ChildInfo
     * @param searchPoiChildInfo SearchPoiChildInfo
     * @return ChildInfo
     */
    private ChildInfo mapSearchPoiChildInfo(final SearchPoiChildInfo searchPoiChildInfo) {
        return new ChildInfo()
                .setName(searchPoiChildInfo.name)
                .setShortName(searchPoiChildInfo.shortName)
                .setRatio(searchPoiChildInfo.ratio)
                .setLabel(searchPoiChildInfo.label)
                .setPoiId(searchPoiChildInfo.poiId)
                .setLocation(new GeoPoint(searchPoiChildInfo.location.lon, searchPoiChildInfo.location.lat))
                .setAddress(searchPoiChildInfo.address);
    }

}
