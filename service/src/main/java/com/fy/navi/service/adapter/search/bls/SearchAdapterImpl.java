package com.fy.navi.service.adapter.search.bls;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_SERVICE_TAG;

import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryReqStartPoints;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryRequestParam;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryResponseParam;
import com.autonavi.gbl.aosclient.observer.ICallBackNavigationEtaquery;
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
import com.autonavi.gbl.search.model.SearchDeepInfoParam;
import com.autonavi.gbl.search.model.SearchDeepInfoResult;
import com.autonavi.gbl.search.model.SearchEnrouteKeywordParam;
import com.autonavi.gbl.search.model.SearchEnrouteResult;
import com.autonavi.gbl.search.model.SearchLineDeepInfoParam;
import com.autonavi.gbl.search.model.SearchLineDeepInfoResult;
import com.autonavi.gbl.search.model.SearchMode;
import com.autonavi.gbl.search.model.SearchNearestParam;
import com.autonavi.gbl.search.model.SearchNearestResult;
import com.autonavi.gbl.search.model.SearchPoiDetailParam;
import com.autonavi.gbl.search.model.SearchResult;
import com.autonavi.gbl.search.model.SearchSuggestionParam;
import com.autonavi.gbl.search.model.SuggestionSearchResult;
import com.autonavi.gbl.util.model.TaskResult;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.adapter.search.ISearchApi;
import com.fy.navi.service.adapter.search.ISearchResultCallback;
import com.fy.navi.service.adapter.search.cloud.http.ApiClient;
import com.fy.navi.service.adapter.search.cloud.http.RxRetrofitClient;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchRequestParameter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import rx.Observable;
import rx.Subscriber;
import rx.android.schedulers.AndroidSchedulers;
import rx.schedulers.Schedulers;


/**
 * 搜索适配器实现类，用于处理各种搜索请求并管理回调。
 */
public class SearchAdapterImpl extends SearchServiceV2Manager implements ISearchApi {

    // 任务ID计数器，确保每个搜索任务都有唯一的ID
    private final AtomicInteger taskId = new AtomicInteger(0);

    // 存储搜索结果回调接口的线程安全列表
    private final List<ISearchResultCallback> mSearchResponseCallbackList = new CopyOnWriteArrayList<>();

    // 搜索观察者类，用于管理搜索回调
    private SearchObserversHelper mSearchObserversHelper;

    // 搜索结果通知类，用于分发搜索结果给注册的回调
    private SearchResultCallbackHelper searchNotificationHelper;

    private BLAosService mBLAosService;

    /**
     * 初始化搜索服务。
     */
    @Override
    public void init() {
        mBLAosService = new BLAosService();
        initService();
        mSearchObserversHelper = SearchObserversHelper.getInstance();
        searchNotificationHelper = new SearchResultCallbackHelper(mSearchResponseCallbackList);
    }

    @Override
    public void unInitSearchApi() {
        mSearchObserversHelper.unInit();
        searchNotificationHelper.unInit();
        unInit();
    }

    /**
     * 注册一个搜索结果回调接口，确保不重复添加相同的回调。
     *
     * @param callback 搜索结果回调接口
     */
    @Override
    public void registerSearchObserver(ISearchResultCallback callback) {
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
    public void unRegisterSearchObserver(ISearchResultCallback callback) {
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
    public int suggestionSearch(SearchRequestParameter requestParameterBuilder) {
        Logger.d(SEARCH_SERVICE_TAG, "suggestionSearch");
        SearchCallbackWrapper<SuggestionSearchResult> callbackWrapper = createCallbackWrapper(
                SuggestionSearchResult.class,
                result -> notifySearchSuccess(taskId.get(), requestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), requestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(SuggestionSearchResult.class, callbackWrapper);
        try {
            SearchSuggestionParam param = SearchRequestParamV2.getInstance().convertToSearchSuggestionParamV2(requestParameterBuilder);
            SearchResult searchResult = getSearchServiceV2().search(param, mSearchObserversHelper);
            taskId.set(searchResult.taskId);
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 执行关键字搜索，并返回任务ID。
     *
     * @param requestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int keyWordSearch(SearchRequestParameter requestParameterBuilder) {
        Logger.d(SEARCH_SERVICE_TAG, "keyWordSearch");
        SearchCallbackWrapper<KeywordSearchResultV2> callbackWrapper = createCallbackWrapper(
                KeywordSearchResultV2.class,
                result -> notifySearchSuccess(taskId.get(), requestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), requestParameterBuilder, result)
        );
        mSearchObserversHelper.registerCallback(KeywordSearchResultV2.class, callbackWrapper);
        try {
            KeywordSearchTQueryParam param = SearchRequestParamV2.getInstance().convertToSearchKeywordParamV2(requestParameterBuilder);
            getSearchServiceV2().keyWordSearchTQuery(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ADVANCED, taskId.incrementAndGet());
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 执行聚合搜索，并返回任务ID。
     *
     * @param searchRequestParameter 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int aggregateSearch(SearchRequestParameter searchRequestParameter) {
        Logger.d(SEARCH_SERVICE_TAG, "aggregateSearch");
        SearchCallbackWrapper<AggregateSearchResult> callbackWrapper = createCallbackWrapper(
                AggregateSearchResult.class,
                result -> notifySearchSuccess(taskId.get(), searchRequestParameter, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), searchRequestParameter, result)
        );
        mSearchObserversHelper.registerCallback(AggregateSearchResult.class, callbackWrapper);
        try {
            SearchAggregateParam param = SearchRequestParamV2.getInstance().convertToSearchAggregateParamV2(searchRequestParameter);
            SearchResult searchResult = getSearchServiceV2().search(param, mSearchObserversHelper);
            taskId.set(searchResult.taskId);
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 执行顺路搜索，并返回任务ID。
     *
     * @param searchRequestParameter 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int enRouteKeywordSearch(SearchRequestParameter searchRequestParameter) {
        Logger.d(SEARCH_SERVICE_TAG, "enRouteKeywordSearch");
        SearchCallbackWrapper<SearchEnrouteResult> callbackWrapper = createCallbackWrapper(
                SearchEnrouteResult.class,
                result -> notifySearchSuccess(taskId.get(), searchRequestParameter, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), searchRequestParameter, result)
        );
        mSearchObserversHelper.registerCallback(SearchEnrouteResult.class, callbackWrapper);
        try {
            PathInfo pathInfo = (PathInfo) searchRequestParameter.getPathInfo();
            Logger.d(SEARCH_SERVICE_TAG, "enRouteKeywordSearch pathInfo: " + pathInfo);
            SearchEnrouteKeywordParam param = SearchRequestParamV2.getInstance().convertToSearchEnRouteKeywordParamV2(searchRequestParameter);
            TaskResult searchResult = getSearchServiceV2().search(pathInfo, param, mSearchObserversHelper);
            taskId.set((int) searchResult.taskId);
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 执行周边搜索，并返回任务ID。
     *
     * @param requestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int aroundSearch(SearchRequestParameter requestParameterBuilder) {
        Logger.d(SEARCH_SERVICE_TAG, "aroundSearch");
        SearchCallbackWrapper<KeywordSearchResultV2> callbackWrapper = createCallbackWrapper(
                KeywordSearchResultV2.class,
                result -> notifySearchSuccess(taskId.get(), requestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), requestParameterBuilder, result)
        );
        mSearchObserversHelper.registerCallback(KeywordSearchResultV2.class, callbackWrapper);
        try {
            KeywordSearchRqbxyParam param = SearchRequestParamV2.getInstance().convertToAroundSearchParam(requestParameterBuilder);
            getSearchServiceV2().keyWordSearchRqbxy(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ADVANCED, taskId.incrementAndGet());
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 云端充电桩搜索
     *
     * @param searchRequestParameter {@link SearchRequestParameter}
     *                               areaCode stationList 必传一个
     *                               lat lng 必须同时传或者不传
     *                               filterList 不为空则 areaCode 必填
     *                               stationList filterList 不能同时存在
     * @return task id
     */
    @Override
    public int queryStationNewResult(SearchRequestParameter searchRequestParameter) {
        Map<String, String> parameters = new HashMap<>();
        parameters.put("areaCode", ""); // 城市 （stationList为空必传
        parameters.put("lat", ""); // 纬度 例:32.489931
        parameters.put("lng", ""); // 经度 例:119.862440
        parameters.put("keyWords", ""); // 地址/充电站名称 模糊查询
        parameters.put("stationList", ""); // 用于用户收藏后查询
        parameters.put("operatorId", ""); // 运营商ID
        parameters.put("stationIds", ""); // 充电站ID
        parameters.put("filterList", ""); // 充电站筛选条
        parameters.put("filterAttr", ""); // 筛选属性
        parameters.put("filterValue", ""); // 筛选值
        parameters.put("stationFlag", ""); // 充电站标识 （A-凯迪拉克专属站,B-奥特能专属站,C类)AB: A&B类站,NAB: 非A/B类站,NA:非A类站
        parameters.put("distance", ""); // 距离（默认单位：公里）
        parameters.put("stationType", ""); // 不传或0代表所有，1为仅对外开放(公共站)，2为不对外开放
        parameters.put("from", ""); // 第N页
        parameters.put("size", ""); // 每页记录数
        parameters.put("stationStatus", ""); // 站点状态 0: 未知；1 : 建设中；5: 关闭下线：6: 维护中；50: 正常使用
        parameters.put("isParkFeeFree", ""); // 停车减免 0：不支持 1：支持
        parameters.put("carOwnerFlag", ""); // 是否支持公共充电 1：支持
        parameters.put("internalUse", ""); // 是否内部站 1：是 不传或传空 默认不查询内部站
        parameters.put("stationTypeFlag", ""); // 场站类型： 商场、写字楼、文体、机场、火车站、景区高速服务区
        parameters.put("parkType", ""); // 车位情况：侧桩、后桩、混
        Observable<PoiInfoEntity> poiInfoEntityObservable = RxRetrofitClient.create(ApiClient.class).queryStationNewResult(parameters);
        queryStationNewResult(taskId.incrementAndGet(), searchRequestParameter, poiInfoEntityObservable);
        return taskId.get();
    }

    private void queryStationNewResult(int taskId, SearchRequestParameter source, Observable<PoiInfoEntity> observable) {
        observable.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Subscriber<PoiInfoEntity>() {
                    @Override
                    public void onStart() {
                        Logger.d(SEARCH_SERVICE_TAG, "---onStart......");
                    }

                    @Override
                    public void onCompleted() {
                        Logger.d(SEARCH_SERVICE_TAG, "---onCompleted......");
                    }

                    @Override
                    public void onError(Throwable e) {
                        Logger.d(SEARCH_SERVICE_TAG, "---onError......" + e.getMessage());
                    }

                    @Override
                    public void onNext(PoiInfoEntity cloudPoiResultBean) {
                        Logger.d(SEARCH_SERVICE_TAG, Thread.currentThread().getName() + "---onNext......" + cloudPoiResultBean.toString());

                    }
                });
    }

    /**
     * 执行POI详情搜索，并返回任务ID。
     *
     * @param requestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int poiDetailSearch(SearchRequestParameter requestParameterBuilder) {
        Logger.d(SEARCH_SERVICE_TAG, "poiDetailSearch");
        SearchCallbackWrapper<PoiDetailSearchResult> callbackWrapper = createCallbackWrapper(
                PoiDetailSearchResult.class,
                result -> notifySearchSuccess(taskId.get(), requestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), requestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(PoiDetailSearchResult.class, callbackWrapper);
        try {
            SearchPoiDetailParam param = SearchRequestParamV2.getInstance().convertToSearchPoiDetailParamV2(requestParameterBuilder);
            getSearchServiceV2().poiDetailSearch(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ONLY, taskId.incrementAndGet());
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 执行POI ID搜索，并返回任务ID。
     *
     * @param searchRequestParameterBuilder 搜索参数构建器{@link SearchRequestParameter}
     * @return 任务ID
     */
    @Override
    public int poiIdSearch(SearchRequestParameter searchRequestParameterBuilder) {
        Logger.d(SEARCH_SERVICE_TAG, "poiIdSearch");
        SearchCallbackWrapper<KeywordSearchResultV2> callbackWrapper = createCallbackWrapper(
                KeywordSearchResultV2.class,
                result -> notifySearchSuccess(taskId.get(), searchRequestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), searchRequestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(KeywordSearchResultV2.class, callbackWrapper);
        try {
            KeywordSearchIdqParam param = SearchRequestParamV2.getInstance().convertToKeywordSearchIdqParam(searchRequestParameterBuilder);
            getSearchServiceV2().keyWordSearchIdq(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ONLY, taskId.incrementAndGet());
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 执行地理坐标搜索，并返回任务ID。
     *
     * @param searchRequestParameterBuilder 搜索参数{@link SearchRequestParameter}
     * @return taskId
     */
    @Override
    public int geoSearch(SearchRequestParameter searchRequestParameterBuilder) {
        Logger.d(SEARCH_SERVICE_TAG, "geoSearch");
        SearchCallbackWrapper<SearchNearestResult> callbackWrapper = createCallbackWrapper(
                SearchNearestResult.class,
                result -> notifySearchSuccess(taskId.get(), searchRequestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), searchRequestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(SearchNearestResult.class, callbackWrapper);

        try {
            SearchNearestParam nearestParam = new SearchNearestParam();
            nearestParam.poi_loc.lon = searchRequestParameterBuilder.getPoiLoc().getLon();
            nearestParam.poi_loc.lat = searchRequestParameterBuilder.getPoiLoc().getLat();
            getSearchServiceV1().nearestSearch(nearestParam, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ONLY, taskId.incrementAndGet());
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 深度信息搜索，并返回任务ID。
     *
     * @param parameter 搜索参数{@link SearchRequestParameter}
     * @return taskId
     */
    @Override
    public int deppInfoSearch(SearchRequestParameter parameter) {
        Logger.d(SEARCH_SERVICE_TAG, "deppInfoSearch");
        SearchCallbackWrapper<SearchDeepInfoResult> callbackWrapper = createCallbackWrapper(
                SearchDeepInfoResult.class,
                result -> notifySearchSuccess(taskId.get(), parameter, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), parameter, result)
        );

        mSearchObserversHelper.registerCallback(SearchDeepInfoResult.class, callbackWrapper);

        try {
            SearchDeepInfoParam param = new SearchDeepInfoParam();
            param.poiid = parameter.getPoiId();
            param.poi_loc.lat = parameter.getPoiLoc().lat;
            param.poi_loc.lon = parameter.getPoiLoc().lat;
            getSearchServiceV1().deepInfoSearch(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ONLY, taskId.incrementAndGet());
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 执行线路深度信息搜索，并返回任务ID。
     *
     * @param parameter 搜索参数{@link SearchRequestParameter}
     * @return taskId
     */
    @Override
    public int doLineDeepInfoSearch(SearchRequestParameter parameter) {
        Logger.d(SEARCH_SERVICE_TAG, "deppInfoSearch");
        SearchCallbackWrapper<SearchLineDeepInfoResult> callbackWrapper = createCallbackWrapper(
                SearchLineDeepInfoResult.class,
                result -> notifySearchSuccess(taskId.get(), parameter, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), parameter, result)
        );

        mSearchObserversHelper.registerCallback(SearchLineDeepInfoResult.class, callbackWrapper);

        try {
            SearchLineDeepInfoParam searchLineDeepInfoParam = new SearchLineDeepInfoParam();
            searchLineDeepInfoParam.poiIds = (java.util.ArrayList<String>) parameter.getPoiIdList();
            searchLineDeepInfoParam.queryType = Integer.parseInt(parameter.getQueryType());
            getSearchServiceV1().lineDeepInfoSearch(searchLineDeepInfoParam, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ONLY, taskId.incrementAndGet());
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
    }

    /**
     * 执行 沿途搜索。
     *
     * @param searchRequestParameterBuilder SearchRequestParameterBuilder{@link SearchRequestParameter}
     * @return taskId
     */
    @Override
    public int alongWaySearch(SearchRequestParameter searchRequestParameterBuilder) {
        Logger.d(SEARCH_SERVICE_TAG, "alongWaySearch");
        SearchCallbackWrapper<SearchAlongWayResult> callbackWrapper = createCallbackWrapper(
                SearchAlongWayResult.class,
                result -> notifySearchSuccess(taskId.get(), searchRequestParameterBuilder, result),
                (errCode, result) -> notifySearchSuccess(taskId.get(), searchRequestParameterBuilder, result)
        );

        mSearchObserversHelper.registerCallback(SearchAlongWayResult.class, callbackWrapper);
        try {
            SearchAlongWayParam param = SearchRequestParamV2.getInstance().convertToAlongWaySearchIdqParam(searchRequestParameterBuilder);
            getSearchServiceV1().alongWaySearch(param, mSearchObserversHelper, SearchMode.SEARCH_MODE_ONLINE_ADVANCED, taskId.incrementAndGet());
        } catch (Exception e) {
            Logger.e(SEARCH_SERVICE_TAG, "Search operation failed due to exception: " + e.getMessage());
        }
        return taskId.get();
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
    public void abortSearch(int taskId) {
        getSearchServiceV2().abort(taskId);
    }


    /**
     * 获取预计到达时间、剩余距离
     *
     * @param searchRequestParameterBuilder SearchRequestParameter
     * @return distance ，travelTime
     */
    @Override
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(SearchRequestParameter searchRequestParameterBuilder) {

        // TODO 后面需要对接真是的能耗模型参数
        //GNavigationEtaqueryRequestParam requestParam = SearchRequestParamV2.getInstance().convertToGNavigationEtaqueryRequestParam(searchRequestParameterBuilder);
        GNavigationEtaqueryRequestParam requestParam = new GNavigationEtaqueryRequestParam();
        requestParam.start.points.add(new GNavigationEtaqueryReqStartPoints(13, 2, searchRequestParameterBuilder.getUserLoc().lon, searchRequestParameterBuilder.getUserLoc().lat));
        requestParam.end.points.add(new GNavigationEtaqueryReqStartPoints(143, 2, searchRequestParameterBuilder.getPoiLoc().lon, searchRequestParameterBuilder.getPoiLoc().lat));

        CompletableFuture<Pair<String, String>> future = new CompletableFuture<>();
        mBLAosService.sendReqNavigationEtaquery(requestParam, response -> {
            if (response.route_list != null && !response.route_list.isEmpty() && response.route_list.get(0).path != null && !response.route_list.get(0).path.isEmpty()) {
                String distance = formatDistanceArrayInternal(response.route_list.get(0).path.get(0).distance);
                String travelTime = TimeUtils.switchHourAndMimuteFromSecond(AppContext.mContext, (int) response.route_list.get(0).path.get(0).travel_time);
                future.complete(new Pair<>(distance, travelTime));
                Logger.d(SEARCH_SERVICE_TAG, "distance:" + distance + " travelTime:" + travelTime);
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
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(SearchRequestParameter searchRequestParameterBuilder) {

        // TODO 后面需要对接真是的能耗模型参数
        //GNavigationEtaqueryRequestParam requestParam = SearchRequestParamV2.getInstance().convertToGNavigationEtaqueryRequestParam(searchRequestParameterBuilder);
        GNavigationEtaqueryRequestParam requestParam = new GNavigationEtaqueryRequestParam();
        requestParam.start.points.add(new GNavigationEtaqueryReqStartPoints(13, 2, searchRequestParameterBuilder.getUserLoc().lon, searchRequestParameterBuilder.getUserLoc().lat));
        requestParam.end.points.add(new GNavigationEtaqueryReqStartPoints(143, 2, searchRequestParameterBuilder.getPoiLoc().lon, searchRequestParameterBuilder.getPoiLoc().lat));

        CompletableFuture<ETAInfo> future = new CompletableFuture<>();
        mBLAosService.sendReqNavigationEtaquery(requestParam, response -> {
            if (response.route_list != null && !response.route_list.isEmpty() && response.route_list.get(0).path != null && !response.route_list.get(0).path.isEmpty()) {
                ETAInfo etaInfo = new ETAInfo()
                        .setDistance(response.route_list.get(0).path.get(0).distance)
                        .setTravelTime(TimeUtils.switchHourAndMimuteFromSecond(AppContext.mContext, (int) response.route_list.get(0).path.get(0).travel_time))
                        .setLeftCharge(response.route_list.get(0).path.get(0).charge_left);
//                String distance = formatDistanceArrayInternal(response.route_list.get(0).path.get(0).distance);
//                String travelTime = TimeUtils.switchHourAndMimuteFromSecond(AppContext.mContext, (int) response.route_list.get(0).path.get(0).travel_time);
                future.complete(etaInfo);
//                Logger.d(SEARCH_SERVICE_TAG, "distance:" + distance + " travelTime:" + travelTime);
            } else {
                future.completeExceptionally(new Exception("No valid route data found"));
            }
        });
        return future;
    }

    private String formatDistanceArrayInternal(int distance) {
        String[] distanceArray = ConvertUtils.formatDistanceArray(AppContext.mContext, distance);
        return distanceArray[0] + distanceArray[1];
    }

    /**
     * 通知所有注册的回调搜索成功，并传递搜索结果。
     *
     * @param requestParameterBuilder 搜索参数构建器
     * @param result                  搜索结果
     * @param <T>                     泛型类型
     */
    private <T> void notifySearchSuccess(int taskId, SearchRequestParameter requestParameterBuilder, T result) {
        searchNotificationHelper.notifySearchSuccess(taskId, requestParameterBuilder, result);
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
            Class<T> resultType,
            Consumer<T> onSuccess,
            BiConsumer<Integer, T> onFailure) {
        return searchNotificationHelper.createCallbackWrapper(resultType, onSuccess, onFailure);
    }

}
