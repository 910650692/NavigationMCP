package com.fy.navi.service.logicpaket.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;
import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_SERVICE_TAG;

import android.text.TextUtils;
import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.layer.bls.manager.SearchLayerStyle;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.mapdata.MapDataAdapter;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.position.PositionAdapter;
import com.fy.navi.service.adapter.route.RouteAdapter;
import com.fy.navi.service.adapter.search.ISearchResultCallback;
import com.fy.navi.service.adapter.search.SearchAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.layer.SearchResultLayer;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchRequestParameter;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.search.SearchRetainParamInfo;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @Author: baipeng0904
 * @Description: 搜索业务层，提供搜索功能给HM、其它模块使用
 */
public class SearchPackage implements ISearchResultCallback, ILayerAdapterCallBack {
    private static final String TAG = "SearchPackage";
    private final HistoryManager mManager;
    private final PositionAdapter mPositionAdapter;
    private final MapDataAdapter mMapDataAdapter;
    private final SearchAdapter mSearchAdapter;
    private final LayerAdapter mLayerAdapter;
    private final MapAdapter mMapAdapter;
    private final RouteAdapter mRouteAdapter;
    private final NavistatusAdapter mNavistatusAdapter;
    private final Map<String, SearchResultCallback> mISearchResultCallbackMap = new LinkedHashMap<>();
    private final AtomicReference<String> mCurrentCallbackId = new AtomicReference<>();

    private SearchPackage() {
        mManager = HistoryManager.getInstance();
        mManager.init();
        mMapAdapter = MapAdapter.getInstance();
        mPositionAdapter = PositionAdapter.getInstance();
        mMapDataAdapter = MapDataAdapter.getInstance();
        mSearchAdapter = SearchAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mRouteAdapter = RouteAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mLayerAdapter.registerLayerClickObserver(MapTypeId.MAIN_SCREEN_MAIN_MAP, LayerType.SEARCH_LAYER, this);
    }

    private static final class SearchPackageHolder {
        private static final SearchPackage INSTANCE = new SearchPackage();
    }

    public static SearchPackage getInstance() {
        return SearchPackageHolder.INSTANCE;
    }

    public void initSearchService() {
        mSearchAdapter.init();
        mSearchAdapter.registerCallBack(this);
    }

    /**
     * SDK 正常搜索结果回调接口
     *
     * @param errorCode          错误码
     * @param message            错误信息
     * @param searchResultEntity 搜索结果列表
     */
    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        Logger.d(SEARCH_SERVICE_TAG, "onSearchResult=> errorCode: {}, message: {}", errorCode, message);
        if (searchResultEntity != null) {
            addPoiMarker(searchResultEntity);
        }
        ThreadManager threadManager = ThreadManager.getInstance();
        for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
            String identifier = entry.getKey();
            mCurrentCallbackId.set(identifier);
            SearchResultCallback callback = entry.getValue();
            threadManager.postUi(() -> callback.onSearchResult(taskId, errorCode, message, searchResultEntity));
        }
    }

    /**
     * 静默搜索结果回调接口
     *
     * @param errorCode          错误码
     * @param message            错误信息
     * @param searchResultEntity 搜索结果列表
     */
    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        Logger.d(SEARCH_SERVICE_TAG, "onSilentSearchResult=> errorCode: {}, message: {}", errorCode, message);
        ThreadManager threadManager = ThreadManager.getInstance();
        for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
            String identifier = entry.getKey();
            mCurrentCallbackId.set(identifier);
            SearchResultCallback callback = entry.getValue();
            threadManager.postUi(() -> callback.onSilentSearchResult(taskId, errorCode, message, searchResultEntity));
        }
    }

    /**
     * 注册 HMI 搜索回调
     *
     * @param callback 回调接口
     */
    public void registerCallBack(String callbackId, SearchResultCallback callback) {
        if (callback == null || callbackId == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to register callback: callback or identifier is null.");
            return;
        }
        if (!mISearchResultCallbackMap.containsKey(callbackId)) {
            mISearchResultCallbackMap.put(callbackId, callback);
        } else {
            Logger.w(SEARCH_SERVICE_TAG, "Callback with identifier {} already registered.", callbackId);
        }
    }

    /**
     * 注销 HMI 搜索回调
     *
     * @param callbackId 回调唯一标识
     */
    public void unRegisterCallBack(String callbackId) {
        if (callbackId == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to unregister callback: identifier is null.");
            return;
        }
        mISearchResultCallbackMap.remove(callbackId);
    }

    /**
     * 获取当前回调的标识符
     *
     * @return 当前回调的标识符
     */
    public String getCurrentCallbackId() {
        return mCurrentCallbackId.get();
    }

    /**
     * 关键字搜索
     *
     * @param keyword 关键字
     * @param page    搜索页数
     * @return taskId
     */
    public int keywordSearch(int page, String keyword) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        Logger.d(SEARCH_SERVICE_TAG, "keywordSearch=> getMapBound(): " + mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP));
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .userLoc(userLoc)
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing keyword search.");
        addSearchKeywordRecord(keyword);
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 关键字离线搜索
     *
     * @param keyword 关键字
     * @param page    搜索页数
     * @return taskId
     */
    public int keywordSearch(int page, String keyword, int adCode) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        Logger.d(SEARCH_SERVICE_TAG, "keywordSearch=> getMapBound(): " + mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP));
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP))
                .adCode(adCode)
                .userLoc(userLoc)
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing keyword search.");
        addSearchKeywordRecord(keyword);
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 关键字搜索2.0
     *
     * @param keyword      关键字
     * @param page         搜索页数
     * @param retain       筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传
     * @param checkedLevel 用户筛选级别
     * @param classifyData 一筛参数
     * @return taskId
     */
    public int keywordSearch(int page, String keyword, String retain, String checkedLevel, String classifyData, boolean isSilentSearch) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        Logger.d(SEARCH_SERVICE_TAG, "keywordSearch=> getMapBound(): " + mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP));
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilentSearch)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP))
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .retainState(retain)
                .checkedLevel(checkedLevel)
                .classifyV2Data(classifyData)
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing keyword search.");
        addSearchKeywordRecord(keyword);
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 关键字静默搜索
     *
     * @param keyword 关键字
     * @param page    搜索页数
     * @return taskId
     */
    public int silentKeywordSearch(int page, String keyword) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }

        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(true)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP))
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing keyword search.");
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 执行预搜索
     *
     * @param keyword 搜索参数{@link SearchRequestParameter}
     */
    public int suggestionSearch(String keyword) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute suggestionSearch: searchRequestParameterBuilder is null.");
            return -1;
        }

        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        Logger.d(SEARCH_HMI_TAG, "userLoc:" + userLoc.toString() + "  mMapDataPackage.getAdcode()" + mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()));
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .keyword(keyword)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_SUGGESTION)
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing suggestion search.");
        return mSearchAdapter.suggestionSearch(requestParameterBuilder);
    }

    /**
     * Poi 详情搜索
     *
     * @param poiInfoEntity         Poi信息实体
     * @param searchRetainParamInfo 服务状态回传参数，来自于关键字搜索结果{@link SearchRetainParamInfo}
     * @return taskId
     */
    public int poiDetailSearch(PoiInfoEntity poiInfoEntity, SearchRetainParamInfo searchRetainParamInfo) {
        if (poiInfoEntity == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute poiDetailSearch: poiInfoEntity is null.");
            return -1;
        }

        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        Logger.d(SEARCH_HMI_TAG, "userLoc:" + userLoc.toString() + "  mMapDataPackage.getAdcode()" + mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()));
        SearchRequestParameter searchRequestParameterBuilder = new SearchRequestParameter.Builder()
                .searchType(AutoMapConstant.SearchType.POI_DETAIL_SEARCH)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .poiId(poiInfoEntity.getPid())
                .userLoc(userLoc)
                .searchRetainParam(searchRetainParamInfo)
                .build();
        return mSearchAdapter.poiDetailSearch(searchRequestParameterBuilder);
    }

    /**
     * Poi Id 搜索
     *
     * @param poiId POI id
     */
    public int poiIdSearch(String poiId) {
        if (poiId == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute poiIdSearch: searchRequestParameterBuilder is null.");
            return -1;
        }

        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        Logger.d(SEARCH_HMI_TAG, "userLoc:" + userLoc.toString() + "  mMapDataPackage.getAdcode()" + mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()));
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .poiId(poiId)
                .queryType(AutoMapConstant.SearchQueryType.ID)
                .searchType(AutoMapConstant.SearchType.POI_SEARCH)
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing poiDetailSearch search.");
        return mSearchAdapter.poiIdSearch(requestParameterBuilder);
    }

    /**
     * Geo 搜索
     *
     * @param geoPoint GeoPoint
     * @return taskId
     */
    public int geoSearch(GeoPoint geoPoint) {
        Logger.d(SEARCH_SERVICE_TAG, "Executing geoSearch search.");
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .searchType(AutoMapConstant.SearchType.GEO_SEARCH)
                .poiLoc(geoPoint)
                .build();
        return mSearchAdapter.geoSearch(requestParameterBuilder);
    }

    /**
     * 深度信息搜索
     *
     * @param geoPoint GeoPoint
     * @param poiId    poiId
     * @return taskId
     */
    public int deppInfoSearch(String poiId, GeoPoint geoPoint) {
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .searchType(AutoMapConstant.SearchType.DEEP_INFO_SEARCH)
                .poiLoc(geoPoint)
                .poiId(poiId)
                .build();
        return mSearchAdapter.deppInfoSearch(requestParameterBuilder);
    }

    /**
     * 沿途批量搜索
     *
     * @param poiIdList poiId list
     * @param keyword   搜索类型(加油站,停车场，服务区) {@link AutoMapConstant.LineDeepQueryType }
     * @return taskId
     */
    public int doLineDeepInfoSearch(String keyword, List<String> poiIdList) {
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .searchType(AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH)
                .queryType(String.valueOf(LineDeepQueryTypeMatcher.getKeywordType(keyword)))
                .poiIdList(poiIdList)
                .build();
        return mSearchAdapter.doLineDeepInfoSearch(requestParameterBuilder);
    }

    /**
     * 周边搜索，需要传入指定的经纬度
     *
     * @param page    页数
     * @param keyword 搜索关键词
     * @return taskId
     */
    public int aroundSearch(int page, String keyword, GeoPoint geoPoint) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute nearby search: searchRequestParameterBuilder is null.");
            return -1;
        }
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.AROUND)
                .searchType(AutoMapConstant.SearchType.AROUND_SEARCH)
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .geoobj(mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing around search.");
        return mSearchAdapter.aroundSearch(requestParameterBuilder);
    }

    /**
     * 周边搜索，默认自车位置附近搜索
     *
     * @param page    页数
     * @param keyword 搜索关键词
     * @return taskId
     */
    public int aroundSearch(int page, String keyword) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute nearby search: searchRequestParameterBuilder is null.");
            return -1;
        }
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.AROUND)
                .searchType(AutoMapConstant.SearchType.AROUND_SEARCH)
                .userLoc(userLoc)
                .poiLoc(userLoc)
                .geoobj(mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing around search.");
        return mSearchAdapter.aroundSearch(requestParameterBuilder);
    }

    /**
     * V1沿途搜索
     *
     * @param page    页数
     * @param keyword 搜索关键词
     * @return taskId
     */
    public int alongWaySearch(int page, String keyword) {
        Logger.d(SEARCH_SERVICE_TAG, "Executing along waySearch search.");

        // 获取当前车辆位置
        GeoPoint userLoc = new GeoPoint(mPositionAdapter.getLastCarLocation().getLongitude(), mPositionAdapter.getLastCarLocation().getLatitude());

        // 获取路径点列表
        List<RouteParam> routeParamList = mRouteAdapter.getAllPoiParamList(MapTypeId.MAIN_SCREEN_MAIN_MAP);

        if (routeParamList == null || routeParamList.size() < 2) {
            Logger.e(SEARCH_SERVICE_TAG, "Invalid route parameters.");
            return -1;
        }

        GeoPoint startPoint = routeParamList.get(0).getRealPos();
        GeoPoint endPoint = routeParamList.get(routeParamList.size() - 1).getRealPos();

        List<GeoPoint> geoPointList = Arrays.asList(startPoint, endPoint);

        Logger.d(SEARCH_SERVICE_TAG, "routeParamList size: " + routeParamList.size());

        SearchRequestParameter requestParameter = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .searchType(AutoMapConstant.SearchType.ALONG_WAY_SEARCH)
                .userLoc(userLoc)
                .geoobj(mMapAdapter.getMapBound(MapTypeId.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .naviType(2)
                .startPoint(startPoint)
                .endPoint(endPoint)
                .geolinePointList(geoPointList)
                .build();

        Logger.d(SEARCH_SERVICE_TAG, "Executing along way search.");
        return mSearchAdapter.alongWaySearch(requestParameter);
    }

    /**
     * V2聚合搜索
     *
     * @param keyword 搜索关键词
     */
    public int aggregateSearch(String keyword) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute aggregate search: searchRequestParameterBuilder is null.");
            return -1;
        }
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .type(AggregateKeywordMatcher.getKeywordType(keyword))
                .searchType(AutoMapConstant.SearchType.AGGREGATE_SEARCH)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .userLoc(userLoc)
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "Executing aggregate search.");
        return mSearchAdapter.aggregateSearch(requestParameterBuilder);
    }

    /**
     * V2顺路搜索
     *
     * @param keyword 搜索关键词
     * @return taskId
     */
    public int enRouteKeywordSearch(String keyword) {
        if (keyword == null) {
            Logger.e(SEARCH_SERVICE_TAG, "Failed to execute en route keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .searchType(AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH)
                .userLoc(userLoc)
                .pathInfo(mRouteAdapter.getCurrentPath(MapTypeId.MAIN_SCREEN_MAIN_MAP).getPathInfo())
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(SEARCH_SERVICE_TAG, "en route keyword search.");
        return mSearchAdapter.enRouteKeywordSearch(requestParameterBuilder);
    }

    /**
     * 添加搜索记录
     *
     * @param keyword 关键字
     */
    public void addSearchKeywordRecord(String keyword) {
        History history = new History();
        history.keyWord = keyword;
        history.type = AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY;
        mManager.insertValue(history);
    }

    /**
     * 获取所有搜索记录
     */
    public List<History> getSearchKeywordRecord() {
        List<History> historyList = mManager.loadHistoryByPage(1, 100);
        Logger.d(SEARCH_SERVICE_TAG, "value:" + GsonUtils.toJson(historyList));
        return historyList;
    }

    /**
     * 清除单个历史记录
     */
    public void clearSearchKeywordRecord(long id) {
        mManager.deleteValue(id);
    }

    /**
     * 清除搜索记录
     */
    public void clearSearchKeywordRecord() {
        mManager.deleteAll();
    }

    /**
     * 取消单个搜索
     */
    public void abortSearch(int taskId) {
        Logger.d(SEARCH_SERVICE_TAG, "Aborting search.");
        mSearchAdapter.abortSearch(taskId);
    }

    /**
     * 取消全部搜索
     */
    public void abortSearch() {
        Logger.d(SEARCH_SERVICE_TAG, "Aborting search.");
        mSearchAdapter.abortSearch();
    }

    /**
     * 获取预计到达时间
     *
     * @param geoPoint 目标点经纬度
     * @return distance ，travelTime
     */
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(GeoPoint geoPoint) {
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .build();

        return mSearchAdapter.getTravelTimeFuture(requestParameterBuilder);
    }

    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(GeoPoint geoPoint) {
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .build();

        return mSearchAdapter.getTravelTimeFutureIncludeChargeLeft(requestParameterBuilder);
    }

    /**
     * 添加 POI 点标记
     *
     * @param searchResultEntity SearchResultEntity
     */
    private void addPoiMarker(SearchResultEntity searchResultEntity) {
        if (ConvertUtils.isEmpty(searchResultEntity)) {
            return;
        }

        switch (searchResultEntity.getSearchType()) {
            case AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH:
            case AutoMapConstant.SearchType.SEARCH_KEYWORD:
                createPoiMarker(searchResultEntity.getPoiList());
                break;
            case AutoMapConstant.SearchType.GEO_SEARCH:
                createLabelMarker(searchResultEntity);
            default:
                break;
        }

        PreviewParams previewParams = new PreviewParams();
        List<PreviewParams.PointD> points = searchResultEntity.getPoiList().stream()
                .filter(poiInfo -> poiInfo.getPoint() != null)
                .map(poiInfo -> new PreviewParams.PointD(poiInfo.getPoint().lon, poiInfo.getPoint().lat))
                .collect(Collectors.toList());

        previewParams.setPoints(points);
        previewParams.setbUseRect(false);
        int screenWidth = ScreenUtils.Companion.getInstance().getScreenWidth();
        int screenHeight = ScreenUtils.Companion.getInstance().getScreenHeight();
        previewParams.setMapBound(new PreviewParams.RectDouble(0, screenWidth, 0, screenHeight));
        mMapAdapter.showPreview(MapTypeId.MAIN_SCREEN_MAIN_MAP, previewParams);
    }

    /**
     * 创建 POI 点标记
     *
     * @param poiList List<PoiInfoEntity>
     */
    private void createPoiMarker(List<PoiInfoEntity> poiList) {
        if (ConvertUtils.isEmpty(poiList)) return;

        SearchResultLayer searchResultLayer = new SearchResultLayer();
        for (int i = 0; i < poiList.size(); i++) {
            SearchResultLayer.ParentPoint parentPoint = createParentPoint(poiList.get(i), i + 1);
            searchResultLayer.getParentPoints().add(parentPoint);
        }
        mLayerAdapter.addSearchPointMarker(MapTypeId.MAIN_SCREEN_MAIN_MAP, searchResultLayer);
    }

    /**
     * 创建父级点标记
     *
     * @param poiInfo PoiInfoEntity
     * @param index   下标
     * @return SearchResultLayer
     */
    private SearchResultLayer.ParentPoint createParentPoint(PoiInfoEntity poiInfo, int index) {
        SearchResultLayer.ParentPoint parentPoint = new SearchResultLayer.ParentPoint();
        parentPoint.id = poiInfo.getPid();
        parentPoint.poiName = poiInfo.getName();
        parentPoint.mPos3D = poiInfo.getPoint();
        parentPoint.index = index;
        String typeCode = poiInfo.getPointTypeCode();
        if (!TextUtils.isEmpty(typeCode)) {
            parseTypeCodes(typeCode, parentPoint);
        }
        return parentPoint;
    }

    /**
     * 解析点标记类型
     *
     * @param typeCode    poi类型 扎标专用
     * @param parentPoint ParentPoint
     */
    private void parseTypeCodes(String typeCode, SearchResultLayer.ParentPoint parentPoint) {
        for (String code : typeCode.split("\\|")) {
            if (code.startsWith("0101")) {
                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_GAS;
            }
            if (code.startsWith("0111")) {
                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_CHARGE;
            }
            if (code.equals("010500")) {
                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_CAR_WASHING;
            }
            if (code.startsWith("05")) {
                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_FOOD;
            }
            if (code.startsWith("1509")) {
                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_PARKING;
            }
            if (code.startsWith("1803")) {
                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_SERVICE_ZONE;
            }
            if (code.startsWith("11")) {
                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_SCENIC;
            }
            return;
        }
        parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_UNKONWN;
    }

    // label点击扎标
    private void createLabelMarker(SearchResultEntity searchResultEntity) {
        if (ConvertUtils.isEmpty(searchResultEntity.getPoiList())) return;
        SearchResultLayer.ChildPoint childPoint = new SearchResultLayer.ChildPoint();
        PoiInfoEntity firstElement = searchResultEntity.getPoiList().get(0);
        childPoint.mPid = firstElement.getPid();
        childPoint.childType = firstElement.getPoiType();
        childPoint.mTypeCode = firstElement.getTypeCode();
        childPoint.mPos3D = firstElement.getPoint();
        childPoint.shortName = firstElement.getShortName();
        Logger.d(SEARCH_SERVICE_TAG, "createLabelMarker position:" + childPoint.mPos3D.toString());
        boolean addResult = mLayerAdapter.addSearchLabelMarker(MapTypeId.MAIN_SCREEN_MAIN_MAP, childPoint);
        Logger.d(SEARCH_SERVICE_TAG, "createLabelMarker result:" + addResult);
    }

    // 清楚label的扎标
    public void clearLabelMark() {
        Logger.d(SEARCH_SERVICE_TAG, "clearLabelMark");
        mLayerAdapter.clearSearchMarks(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        mLayerAdapter.clearSearchAllItemLayer(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }

    public void unInitSearchService() {
        mLayerAdapter.unRegisterLayerClickObserver(MapTypeId.MAIN_SCREEN_MAIN_MAP, LayerType.SEARCH_LAYER, this);
        mSearchAdapter.unInit();
    }

    /**
     * 聚合搜索类型匹配，对应SDK类型 {@link AutoMapConstant.AggregateKeywordType}
     */
    static class AggregateKeywordMatcher {
        private static final Map<Pattern, Integer> keywordPatternMap = new LinkedHashMap<>();

        static {
            keywordPatternMap.put(Pattern.compile("美食|餐厅"), AutoMapConstant.AggregateKeywordType.DINING);
            keywordPatternMap.put(Pattern.compile("景点|旅游"), AutoMapConstant.AggregateKeywordType.SCENIC);
            keywordPatternMap.put(Pattern.compile("商圈|购物"), AutoMapConstant.AggregateKeywordType.MALL);
            keywordPatternMap.put(Pattern.compile("充电|充电站"), AutoMapConstant.AggregateKeywordType.CHARGING);
            keywordPatternMap.put(Pattern.compile("停车|停车场"), AutoMapConstant.AggregateKeywordType.PARKING);
            keywordPatternMap.put(Pattern.compile("厕所|卫生间"), AutoMapConstant.AggregateKeywordType.BATHROOM);
        }

        public static int getKeywordType(String text) {
            if (text == null || text.trim().isEmpty()) {
                return AutoMapConstant.AggregateKeywordType.UNKNOWN;
            }
            for (Map.Entry<Pattern, Integer> entry : keywordPatternMap.entrySet()) {
                if (entry.getKey().matcher(text).find()) {
                    return entry.getValue();
                }
            }
            return AutoMapConstant.AggregateKeywordType.UNKNOWN;
        }
    }

    /**
     * 沿途批量搜索型匹配，对应SDK类型 {@link AutoMapConstant.LineDeepQueryType}
     */
    static class LineDeepQueryTypeMatcher {
        private static final Map<Pattern, Integer> keywordPatternMap = new LinkedHashMap<>();

        static {
            keywordPatternMap.put(Pattern.compile("服务|服务区"), AutoMapConstant.LineDeepQueryType.SERVICE_AREA);
            keywordPatternMap.put(Pattern.compile("加油|加油站"), AutoMapConstant.LineDeepQueryType.GAS_STATION);
            keywordPatternMap.put(Pattern.compile("充电|充电站"), AutoMapConstant.LineDeepQueryType.CHARGING);
            keywordPatternMap.put(Pattern.compile("停车|停车场"), AutoMapConstant.LineDeepQueryType.PARK_COMMEND);
        }

        public static int getKeywordType(String text) {
            if (text == null || text.trim().isEmpty()) {
                return AutoMapConstant.LineDeepQueryType.UNKNOWN;
            }
            for (Map.Entry<Pattern, Integer> entry : keywordPatternMap.entrySet()) {
                if (entry.getKey().matcher(text).find()) {
                    return entry.getValue();
                }
            }
            return AutoMapConstant.LineDeepQueryType.UNKNOWN;
        }
    }

    /**
     * 判断是否为算路态或导航态，用来判断搜索页面是否需要添加途经点。
     *
     * @return true为导航态或算路态，false为非导航态或算路态
     */
    public boolean isAlongWaySearch() {
        String currentNaviStatus = mNavistatusAdapter.getCurrentNaviStatus();
        Set<String> validStatuses = new HashSet<>(Set.of(
                NaviStatus.NaviStatusType.NAVING,
                NaviStatus.NaviStatusType.LIGHT_NAVING,
                NaviStatus.NaviStatusType.ROUTING,
                NaviStatus.NaviStatusType.SELECT_ROUTE
        ));
        return validStatuses.contains(currentNaviStatus);
    }


    /**
     * 获取当前位置
     *
     * @return taskId
     */
    public int currentLocationSearch() {
        GeoPoint point = new GeoPoint();
        point.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        point.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        Logger.d(SEARCH_SERVICE_TAG, "getCurrentLocation lon:" + point.getLon() + "lat:" + point.getLat());
        return geoSearch(point);
    }

    public int getAcCode() {
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        return mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat());
    }

    public GeoPoint getCurrentLocation() {
        GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        return userLoc;
    }


    public int getPointTypeCode(String typeCode) {
        for (String code : typeCode.split("\\|")) {
            if (code.startsWith("0101")) {
                return AutoMapConstant.PointTypeCode.GAS_STATION;
            }
            if (code.startsWith("0111")) {
                return AutoMapConstant.PointTypeCode.CHARGING_STATION;
            }
            if (code.equals("010500")) {
                return AutoMapConstant.PointTypeCode.CAR_WASH;
            }
            if (code.startsWith("05")) {
                return AutoMapConstant.PointTypeCode.CATERING;
            }
            if (code.startsWith("1509")) {
                return AutoMapConstant.PointTypeCode.PARKING_LOT;
            }
            if (code.startsWith("1803")) {
                return AutoMapConstant.PointTypeCode.SERVICE_AREA;
            }
            if (code.startsWith("11")) {
                return AutoMapConstant.PointTypeCode.SCENIC_SPOT;
            }
            if (code.startsWith("15")) {
                return AutoMapConstant.PointTypeCode.TRANSPORT_HUB;
            }
        }
        return AutoMapConstant.PointTypeCode.OTHERS;
    }

    // TODO 搜索结果扎标点击回调
    @Override
    public void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        ILayerAdapterCallBack.super.onNotifyClick(mapTypeId, layer, pItem);
        Logger.d(TAG, "onNotifyClick");
        if (pItem.getClickBusinessType() == GemLayerClickBusinessType.BizSearchTypePoiParentPoint) {
            mLayerAdapter.setSearchSelect(mapTypeId, pItem.getClickBusinessType(), String.valueOf(pItem.getIndex()), true);
        }
    }
}