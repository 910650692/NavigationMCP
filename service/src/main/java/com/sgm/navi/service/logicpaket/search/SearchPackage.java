package com.sgm.navi.service.logicpaket.search;


import android.text.TextUtils;
import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.path.model.RestAreaInfo;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.mapdata.MapDataAdapter;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.adapter.position.PositionAdapter;
import com.sgm.navi.service.adapter.route.RouteAdapter;
import com.sgm.navi.service.adapter.search.ISearchResultCallback;
import com.sgm.navi.service.adapter.search.SearchAdapter;
import com.sgm.navi.service.adapter.user.usertrack.UserTrackAdapter;
import com.sgm.navi.service.adapter.search.cloudByPatac.bean.SavedStations;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemSearchResult;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.search.ChildInfo;
import com.sgm.navi.service.define.search.ConnectorInfoItem;
import com.sgm.navi.service.define.search.ETAInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.ReservationInfo;
import com.sgm.navi.service.define.search.SearchRequestParameter;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.search.SearchRetainParamInfo;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.sgm.navi.service.define.user.usertrack.SearchHistoryItemBean;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.greendao.history.HistoryManager;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import lombok.Getter;


final public class SearchPackage implements ISearchResultCallback, ILayerAdapterCallBack {
    private static final String TAG = "SearchPackage";
    private static final String KEY_SEARCH_EXECUTING = "Executing keyword search.";
    private final HistoryManager mManager;
    private final PositionAdapter mPositionAdapter;
    private final MapDataAdapter mMapDataAdapter;
    private final SearchAdapter mSearchAdapter;
    private final LayerAdapter mLayerAdapter;
    private final MapPackage mMapPackage;
    private final RouteAdapter mRouteAdapter;
    private final NavistatusAdapter mNavistatusAdapter;
    private final UserTrackAdapter mUserTrackAdapter;
    private final NaviPackage mNaviPackage;
    private CommonManager mCommonManager;
    private final ConcurrentHashMap<String, SearchResultCallback> mISearchResultCallbackMap = new ConcurrentHashMap<>();
    private final AtomicReference<String> mCurrentCallbackId = new AtomicReference<>();
    private static final Map<LayerPointItemType, LayerItemSearchResult> sMarkerInfoMap = new ConcurrentHashMap<>();
    @Getter
    private boolean mIsShow;
    private ScheduledFuture mScheduledFuture;
    private String mReservationPreNum = "";

    private SearchPackage() {
        mManager = HistoryManager.getInstance();
        mManager.init();
        mMapPackage = MapPackage.getInstance();
        mPositionAdapter = PositionAdapter.getInstance();
        mMapDataAdapter = MapDataAdapter.getInstance();
        mSearchAdapter = SearchAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mRouteAdapter = RouteAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mUserTrackAdapter = UserTrackAdapter.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
        if (mLayerAdapter != null) mLayerAdapter.registerLayerClickObserver(MapType.MAIN_SCREEN_MAIN_MAP,  this);
    }

    private static final class SearchPackageHolder {
        private static final SearchPackage INSTANCE = new SearchPackage();
    }

    public static SearchPackage getInstance() {
        return SearchPackageHolder.INSTANCE;
    }


    /**
     * 初始化搜索服务
     */
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
    public void onSearchResult(final int taskId, final int errorCode, final String message,
                               final SearchResultEntity searchResultEntity, final SearchRequestParameter requestParameter) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "onSearchResult=> errorCode: {}, message: {}", errorCode, message);
        for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
            final String identifier = entry.getKey();
            mCurrentCallbackId.set(identifier);
            final SearchResultCallback callback = entry.getValue();
            callback.onSearchResult(taskId, errorCode, message, searchResultEntity);
        }
        if (searchResultEntity != null) {
            addPoiMarker(searchResultEntity, requestParameter);
            sentBuryPointForSearch(searchResultEntity.getKeyword());
        }
    }

    @Override
    public void onTipDialog(String status) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "onTipDialog");
        for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
            final String identifier = entry.getKey();
            mCurrentCallbackId.set(identifier);
            final SearchResultCallback callback = entry.getValue();
            callback.onTipDialog(status);
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
    public void onSilentSearchResult(final int taskId,final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "onSilentSearchResult=> errorCode: {}, message: {}", errorCode, message);
        final ThreadManager threadManager = ThreadManager.getInstance();
        for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
            threadManager.postUi(() -> {
                final String identifier = entry.getKey();
                mCurrentCallbackId.set(identifier);
                final SearchResultCallback callback = entry.getValue();
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "onSearchResult=> key: " + identifier + " ,value: " + callback.getClass().getSimpleName());
                callback.onSilentSearchResult(taskId, errorCode, message, searchResultEntity);
            });
        }
    }

    /**
     * 网络搜索结果成功回调
     * @param taskId 任务ID
     * @param searchKey 任务来源
     * @param result 搜索结果
     */
    @Override
    public void onNetSearchResult(int taskId, String searchKey, BaseRep result) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "onNetSearchResult");
        final ThreadManager threadManager = ThreadManager.getInstance();
        for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
            final String identifier = entry.getKey();
            mCurrentCallbackId.set(identifier);
            final SearchResultCallback callback = entry.getValue();
            callback.onNetSearchResult(taskId,searchKey,result);
        }
    }

    /**
     * 网络搜索结果失败回调
     * @param taskId 任务ID
     * @param searchKey 任务来源
     * @param message 错误消息
     */
    @Override
    public void onNetSearchResultError(int taskId, String searchKey, String message) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "onNetSearchResultError");
        for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
            final String identifier = entry.getKey();
            mCurrentCallbackId.set(identifier);
            final SearchResultCallback callback = entry.getValue();
            callback.onNetSearchResultError(taskId,searchKey,message);
        }
    }


    /**
     * 注册 HMI 搜索回调
     * @param callbackId 回调唯一标识
     * @param callback 回调接口
     */
    public void registerCallBack(final String callbackId, final SearchResultCallback callback) {
        if (callback == null || callbackId == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to register callback: callback or identifier is null.");
            return;
        }
        if (!mISearchResultCallbackMap.containsKey(callbackId)) {
            mISearchResultCallbackMap.put(callbackId, callback);
        } else {
            Logger.w(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Callback with identifier {} already registered.", callbackId);
        }
    }

    /**
     * 注销 HMI 搜索回调
     *
     * @param callbackId 回调唯一标识
     */
    public void unRegisterCallBack(final String callbackId) {
        if (callbackId == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to unregister callback: identifier is null.");
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
     * @param isSilent 是否静默搜
     * @return taskId
     */
    public int keywordSearch(final int page, final String keyword, final boolean isSilent) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "keyword is null");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilent)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .userLoc(userLoc)
                .checkedLevel("")
                .classifyV2Data("")
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, KEY_SEARCH_EXECUTING);
        addSearchKeywordRecord(keyword);
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 关键字搜索
     *
     * @param keyword 关键字
     * @param page    搜索页数
     * @param isSilent 是否静默搜
     * @param isReSearch 是否触发重搜
     * @return taskId
     */
    public int keywordSearch(final int page, final String keyword, final boolean isSilent, final boolean isReSearch) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "keyword is null");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilent)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .userLoc(userLoc)
                .isReSearch(isReSearch)
                .checkedLevel("")
                .classifyV2Data("")
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, KEY_SEARCH_EXECUTING);
        addSearchKeywordRecord(keyword);
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 关键字离线搜索
     *
     * @param keyword 关键字
     * @param page    搜索页数
     * @param adCode 地理编码
     * @param isSilent 是否静默搜
     * @param isReSearch 是否重搜
     * @return taskId
     */
    public int keywordSearch(final int page, final String keyword, final int adCode, final boolean isSilent, final boolean isReSearch) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "keyword is null");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilent)
                .keyword(keyword)
                .page(page)
                .isReSearch(isReSearch)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(adCode)
                .userLoc(userLoc)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, KEY_SEARCH_EXECUTING);
        addSearchKeywordRecord(keyword);
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 关键字搜索2.0
     *
     * @param keyword      关键字
     * @param page         搜索页数
     * @param retain       筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传
     * @param classifyData 一筛参数
     * @param isSilentSearch 是否静默搜索
     * @return taskId
     */
    public int keywordSearch(final int page, final String keyword, final String retain,
                             final String classifyData, final boolean isSilentSearch) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilentSearch)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .retainState(retain)
                .checkedLevel("1")
                .classifyV2Data(classifyData)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, KEY_SEARCH_EXECUTING);
        addSearchKeywordRecord(keyword);
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 关键字搜索2.0(二筛版本)
     *
     * @param keyword      关键字
     * @param page         搜索页数
     * @param retain       筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传
     * @param classifyData 二筛参数
     * @param isSilentSearch 是否静默搜索
     * @return taskId
     */
    public int keywordSearchByQuickFilter(final int page, final String keyword, final String retain,
                             final String classifyData, final boolean isSilentSearch) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilentSearch)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .retainState(retain)
                .checkedLevel("2")
                .classifyV2Level2Data(classifyData)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, KEY_SEARCH_EXECUTING);
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
    public int silentKeywordSearch(final int page, final String keyword) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }

        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(true)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_KEYWORD)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, KEY_SEARCH_EXECUTING);
        return mSearchAdapter.keywordSearch(requestParameterBuilder);
    }

    /**
     * 执行预搜索
     *
     * @param keyword 搜索参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int suggestionSearch(final String keyword) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute suggestionSearch: searchRequestParameterBuilder is null.");
            return -1;
        }

        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        if (Logger.openLog) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "userLoc:" + userLoc.toString()
                    + "  mMapDataPackage.getAdcode()" + mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()));
        }
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .keyword(keyword)
                .queryType(AutoMapConstant.SearchQueryType.NORMAL)
                .searchType(AutoMapConstant.SearchType.SEARCH_SUGGESTION)
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing suggestion search.");
        return mSearchAdapter.suggestionSearch(requestParameterBuilder);
    }

    /**
     * Poi 详情搜索
     *
     * @param poiInfoEntity         Poi信息实体
     * @param searchRetainParamInfo 服务状态回传参数，来自于关键字搜索结果{@link SearchRetainParamInfo}
     * @return taskId
     */
    public int poiDetailSearch(final PoiInfoEntity poiInfoEntity, final SearchRetainParamInfo searchRetainParamInfo) {
        if (poiInfoEntity == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute poiDetailSearch: poiInfoEntity is null.");
            return -1;
        }

        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter searchRequestParameterBuilder = new SearchRequestParameter.Builder()
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
     * @return taskId
     */
    public int poiIdSearch(final String poiId) {
        if (poiId == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute poiIdSearch: searchRequestParameterBuilder is null.");
            return -1;
        }

        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        if (Logger.openLog) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "userLoc:" + userLoc.toString()
                    + "  mMapDataPackage.getAdcode()" + mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()));
        }
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .poiId(poiId)
                .queryType(AutoMapConstant.SearchQueryType.ID)
                .searchType(AutoMapConstant.SearchType.POI_SEARCH)
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing poiDetailSearch search." + poiId);
        return mSearchAdapter.poiIdSearch(requestParameterBuilder);
    }

    /**
     * Poi Id 搜索
     *
     * @param poiId POI id
     * @return taskId
     */
    public int enRoutePoiIdSearch(final String poiId) {
        if (poiId == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute poiIdSearch: searchRequestParameterBuilder is null.");
            return -1;
        }

        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        if (Logger.openLog) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "userLoc:" + userLoc.toString()
                    + "  mMapDataPackage.getAdcode()" + mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()));
        }
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .poiId(poiId)
                .queryType(AutoMapConstant.SearchQueryType.ID)
                .searchType(AutoMapConstant.SearchType.PID_EN_ROUTE_SEARCH)
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing poiDetailSearch search." + poiId);
        return mSearchAdapter.poiIdSearch(requestParameterBuilder);
    }

    /**
     * Poi Id 搜索
     *
     * @param poiId POI id
     * @return taskId
     */
    public int poiIdSearch(final String poiId, final GeoPoint geoPoint) {
        if (poiId == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute poiIdSearch: searchRequestParameterBuilder is null.");
            return -1;
        }

        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        if (Logger.openLog) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "userLoc:" + userLoc.toString()
                    + "  mMapDataPackage.getAdcode()" + mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()));
        }
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .poiId(poiId)
                .queryType(AutoMapConstant.SearchQueryType.ID)
                .searchType(AutoMapConstant.SearchType.POI_SEARCH)
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing poiDetailSearch search." + poiId);
        return mSearchAdapter.poiIdSearch(requestParameterBuilder);
    }

    /**
     * Poi Id 搜索
     *
     * @param poiId POI id
     * @param isSilent 是否静默
     * @return taskId
     */
    public int poiIdSearch(final String poiId, final boolean isSilent) {
        if (poiId == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute poiIdSearch: searchRequestParameterBuilder is null.");
            return -1;
        }

        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        if (Logger.openLog) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "userLoc:" + userLoc.toString()
                    + "  mMapDataPackage.getAdcode()" + mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()));
        }
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilent)
                .poiId(poiId)
                .queryType(AutoMapConstant.SearchQueryType.ID)
                .searchType(AutoMapConstant.SearchType.POI_SEARCH)
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing poiDetailSearch search." + poiId);
        return mSearchAdapter.poiIdSearch(requestParameterBuilder);
    }

    /**
     * Geo 搜索
     *
     * @param geoPoint GeoPoint
     * @return taskId
     */
    public int geoSearch(final GeoPoint geoPoint) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing geoSearch search.");
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .searchType(AutoMapConstant.SearchType.GEO_SEARCH)
                .poiLoc(geoPoint)
                .build();
        return mSearchAdapter.geoSearch(requestParameterBuilder);
    }

    /**
     * Geo 搜索
     *
     * @param geoPoint GeoPoint
     * @param isSilent 是否静默
     * @return taskId
     */
    public int geoSearch(final GeoPoint geoPoint, final boolean isSilent) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing geoSearch search.");
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilent)
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
    public int deppInfoSearch(final String poiId, final GeoPoint geoPoint) {
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
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
    public int doLineDeepInfoSearch(final String keyword, final List<String> poiIdList) {
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
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
     * @param geoPoint 经纬度
     * @param isTerminal 是否是终点停车场搜索
     * @return taskId
     */
    public int aroundSearch(final int page, final String keyword, final GeoPoint geoPoint, final boolean isTerminal) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute nearby search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.AROUND)
                .searchType(isTerminal ? AutoMapConstant.SearchType.TERMINAL_PARK_AROUND_SEARCH : AutoMapConstant.SearchType.AROUND_SEARCH)
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing around search.");
        return mSearchAdapter.aroundSearch(requestParameterBuilder);
    }

    /**
     * 周边搜索，需要传入指定的经纬度和搜索半径
     *
     * @param page    页数
     * @param keyword 搜索关键词
     * @param geoPoint 经纬度
     * @param  range 搜索半径
     * @return taskId
     */
    public int reAroundSearch(final int page, final String keyword, final GeoPoint geoPoint, final String range) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute nearby search: keyword is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .isReSearch(true)
                .queryType(AutoMapConstant.SearchQueryType.AROUND)
                .searchType(AutoMapConstant.SearchType.AROUND_SEARCH)
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .range(range)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing around search with range.");
        return mSearchAdapter.aroundSearch(requestParameterBuilder);
    }

    /**
     * 周边搜索，需要传入指定的经纬度和搜索半径和是否静默搜索
     *
     * @param page    页数
     * @param keyword 搜索关键词
     * @param geoPoint 经纬度
     * @param  range 搜索半径
     * @param isSilentSearch 是否静默搜索
     * @return taskId
     */
    public int aroundSearch(final int page, final String keyword, final GeoPoint geoPoint,
                            final String range, final boolean isSilentSearch) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute nearby search: keyword is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilentSearch)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.AROUND)
                .searchType(AutoMapConstant.SearchType.AROUND_SEARCH)
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .range(range)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing around search with range.");
        return mSearchAdapter.aroundSearch(requestParameterBuilder);
    }

    /**
     * 算路终点周边搜索，需要传入指定的经纬度和搜索半径和是否静默搜索
     *
     * @param page    页数
     * @param keyword 搜索关键词
     * @param geoPoint 经纬度
     * @param  range 搜索半径
     * @param isSilentSearch 是否静默搜索
     * @return taskId
     */
    public int routeTerminalAroundSearch(final int page, final String keyword, final GeoPoint geoPoint,
                            final String range, final boolean isSilentSearch) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute nearby search: keyword is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilentSearch)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.AROUND)
                .searchType(AutoMapConstant.SearchType.ROUTE_TERMINAL_PARK_SEARCH)
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .range(range)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing around search with range.");
        return mSearchAdapter.aroundSearch(requestParameterBuilder);
    }

    /**
     * 周边筛选搜索2.0
     *
     * @param keyword      关键字
     * @param page         搜索页数
     * @param retain       筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传
     * @param classifyData 一筛参数
     * @param isSilentSearch 是否静默搜索
     * @return taskId
     */
    public int aroundSearch(final int page, final String keyword, final String retain,
                             final String classifyData, final boolean isSilentSearch, final GeoPoint geoPoint) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilentSearch)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.AROUND)
                .searchType(AutoMapConstant.SearchType.AROUND_SEARCH)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .retainState(retain)
                .poiLoc(geoPoint)
                .checkedLevel("1")
                .classifyV2Data(classifyData)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing around search with range.");
        return mSearchAdapter.aroundSearch(requestParameterBuilder);
    }

    /**
     * 周边搜索，默认自车位置附近搜索
     *
     * @param page    页数
     * @param keyword 搜索关键词
     * @return taskId
     */
    public int aroundSearch(final int page, final String keyword) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute nearby search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .queryType(AutoMapConstant.SearchQueryType.AROUND)
                .searchType(AutoMapConstant.SearchType.AROUND_SEARCH)
                .userLoc(userLoc)
                .poiLoc(userLoc)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing around search with userLoc.");
        return mSearchAdapter.aroundSearch(requestParameterBuilder);
    }

    /**
     * V1沿途搜索
     *
     * @param page    页数
     * @param keyword 搜索关键词
     * @return taskId
     */
    public int alongWaySearch(final int page, final String keyword) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing along waySearch search.");

        // 获取当前车辆位置
        final GeoPoint userLoc = new GeoPoint(mPositionAdapter.getLastCarLocation().getLongitude(),
                mPositionAdapter.getLastCarLocation().getLatitude());

        // 获取路径点列表
        final List<RouteParam> routeParamList = mRouteAdapter.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);

        if (routeParamList == null || routeParamList.size() < 2) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Invalid route parameters.");
            return -1;
        }

        final GeoPoint startPoint = routeParamList.get(0).getRealPos();
        final GeoPoint endPoint = routeParamList.get(routeParamList.size() - 1).getRealPos();

        final List<GeoPoint> geoPointList = Arrays.asList(startPoint, endPoint);

        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "routeParamList size: " + routeParamList.size());

        final SearchRequestParameter requestParameter = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .page(page)
                .searchType(AutoMapConstant.SearchType.ALONG_WAY_SEARCH)
                .userLoc(userLoc)
                .geoobj(mMapPackage.getMapBound(MapType.MAIN_SCREEN_MAIN_MAP))
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .naviType(2)
                .startPoint(startPoint)
                .endPoint(endPoint)
                .geolinePointList(geoPointList)
                .build();

        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing along way search.");
        return mSearchAdapter.alongWaySearch(requestParameter);
    }

    /**
     * V2聚合搜索
     *
     * @param keyword 搜索关键词
     * @return taskId
     */
    public int aggregateSearch(final String keyword) {
        if (keyword == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute aggregate search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .type(AggregateKeywordMatcher.getKeywordType(keyword))
                .searchType(AutoMapConstant.SearchType.AGGREGATE_SEARCH)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .userLoc(userLoc)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Executing aggregate search.");
        return mSearchAdapter.aggregateSearch(requestParameterBuilder);
    }

    /**
     * V2顺路搜索
     *
     * @param keyword 搜索关键词
     * @return taskId
     */
    public int enRouteKeywordSearch(final String keyword) {
        if (keyword == null || null == mRouteAdapter.getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute en route keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(false)
                .keyword(keyword)
                .searchType(AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH)
                .userLoc(userLoc)
                .pathInfo(mRouteAdapter.getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP).getMPathInfo())
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "en route keyword search.");
        return mSearchAdapter.enRouteKeywordSearch(requestParameterBuilder);
    }

    /**
     * V2顺路搜索
     *
     * @param keyword 搜索关键词
     * @param isSilentSearch 是否静默搜索
     * @return taskId
     */
    public int enRouteKeywordSearch(final String keyword, final boolean isSilentSearch) {
        if (keyword == null || null == mRouteAdapter.getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute en route keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilentSearch)
                .keyword(keyword)
                .searchType(AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH)
                .userLoc(userLoc)
                .pathInfo(mRouteAdapter.getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP).getMPathInfo())
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "en route keyword search.");
        return mSearchAdapter.enRouteKeywordSearch(requestParameterBuilder);
    }

    /**
     * V2顺路搜索2.0
     * @param keyword      关键字
     * @param retain       筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传
     * @param classifyData 一筛参数
     * @param isSilentSearch 是否静默搜索
     * @return
     */
    public int enRouteKeywordSearch(final String keyword, final String retain,
                                    final String classifyData, final boolean isSilentSearch) {
        if (keyword == null || null == mRouteAdapter.getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP)) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Failed to execute en route keyword search: searchRequestParameterBuilder is null.");
            return -1;
        }
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilentSearch)
                .keyword(keyword)
                .searchType(AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH)
                .userLoc(userLoc)
                .retainState(retain)
                .checkedLevel("1")
                .classifyV2Data(classifyData)
                .pathInfo(mRouteAdapter.getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP).getMPathInfo())
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "en route keyword search.");
        return mSearchAdapter.enRouteKeywordSearch(requestParameterBuilder);
    }

    /**
     *  pid批量搜索
     * @param pidList   pid列表
     * @param scene scene推荐场景取值， 0: 无效场景，1: 搜索详情页场景，2：路线规划页场景，3：导航结束页场景，4：导航中
     * @param isSilent 是否静默搜
     * @return taskId
     */
    public int poiListSearch(final List<String> pidList,final int scene, final boolean isSilent) {
        if (ConvertUtils.isEmpty(pidList)) {
            return 0;
        }
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .isSilentSearch(isSilent)
                .searchType(AutoMapConstant.SearchType.PID_LIST_SEARCH)
                .poiIdList(pidList)
                .scene(scene)
                .build();
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "poiListSearch.");
        return mSearchAdapter.poiListSearch(requestParameterBuilder);
    }

    /**
     * 添加搜索记录
     *
     * @param keyword 关键字
     */
    public void addSearchKeywordRecord(final String keyword) {
//        final History history = new History();
//        history.setMKeyWord(keyword);
//        history.setMType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY);
//        mManager.insertValue(history);
    }

    /**
     * @return 所有搜索记录
     */
    public List<History> getSearchKeywordRecord() {
        if (isLogin()) {
            final ArrayList<SearchHistoryItemBean> searchHistoryItemBeans = mUserTrackAdapter.getSearchHistory();
            final List<History> searchHistoryList = Optional.ofNullable(searchHistoryItemBeans)
                    .orElse(new ArrayList<>())
                    .stream()
                    .map(this::convertSearchHistoryItemBeanToHistory)
                    .collect(Collectors.toList());

            final ArrayList<HistoryRouteItemBean> historyRouteItemBeans = mUserTrackAdapter.getHistoryRoute();
            final List<History> historyList = Optional.ofNullable(historyRouteItemBeans)
                    .orElse(new ArrayList<>())
                    .stream()
                    .map(this::convertHistoryRouteItemBeanToHistory)
                    .collect(Collectors.toList());

            final List<History> combinedList = new ArrayList<>(searchHistoryList);
            combinedList.addAll(historyList);
            if (!ConvertUtils.isEmpty(combinedList)) {
                combinedList.sort(Comparator.comparing(History::getMUpdateTime).reversed());
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "size :" + combinedList.size());
            }
            return combinedList;

        } else {
            //离线时数据直接从数据库获取
            final List<History> historyList = mManager.loadHistoryByPage(1, 100);
            if (!ConvertUtils.isEmpty(historyList)) {
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "size :" + historyList.size());
            }
            return historyList;
        }
    }

    /**
     * @return 导航记录
     */
    public List<History> getNaviRecord() {
        if (isLogin()) {
            final ArrayList<HistoryRouteItemBean> historyRouteItemBeans = mUserTrackAdapter.getHistoryRoute();
            final List<History> historyList = Optional.ofNullable(historyRouteItemBeans)
                    .orElse(new ArrayList<>())
                    .stream()
                    .map(this::convertHistoryRouteItemBeanToHistory)
                    .collect(Collectors.toList());
            if (!ConvertUtils.isEmpty(historyList)) {
                historyList.sort(Comparator.comparing(History::getMUpdateTime).reversed());
                historyList.stream().filter(history -> !ConvertUtils.isEmpty(history.getMKeyWord()));
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "size :" + historyList.size());
            }
            return historyList;
        } else {
            final List<History> historyList = mManager.getValueByType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY);
            if (!ConvertUtils.isEmpty(historyList)) {
                historyList.sort(Comparator.comparing(History::getMUpdateTime).reversed());
                historyList.stream().filter(history -> !ConvertUtils.isEmpty(history.getMKeyWord()));
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "size :" + historyList.size());
            }
            return historyList;
        }
    }

    private History convertSearchHistoryItemBeanToHistory(final SearchHistoryItemBean searchHistoryItemBean) {
        final History history = new History();
        history.setMKeyWord(searchHistoryItemBean.getName());
        history.setMType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY);
        history.setMUpdateTime(new Date(searchHistoryItemBean.getUpdateTime()));
        return history;
    }

    private History convertHistoryRouteItemBeanToHistory(final HistoryRouteItemBean item) {
        final History history = new History();
        history.setMType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY);
        history.setMPoiId(item.getToPoi().getPoiId());
        history.setMNaviHistoryId(item.getId());
        history.setMStartPoint(item.getStartLoc().toString());
        history.setMEndPoint(item.getEndLoc().toString());
        history.setMStartPoiName(item.getFromPoi().getName());
        history.setMEndPoiName(item.getToPoi().getName());
        history.setMKeyWord(item.getToPoi().getName());
        history.setMIsCompleted(item.getIsCompleted());
        history.setMUpdateTime(item.getUpdateTime() == 0 ? new Date() : new Date(item.getUpdateTime()));
        return history;
    }

    /**
     * @param id 记录id
     * 清除单个历史记录
     */
    public void clearSearchKeywordRecord(final long id) {
        mManager.deleteValue(id);
    }

    /**
     * 清除搜索记录
     */
    public void clearSearchKeywordRecord() {
        mManager.deleteAll();
    }

    /**
     * @param taskId 任务id
     * 取消单个搜索
     */
    public void abortSearch(final int taskId) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Aborting search.");
        mSearchAdapter.abortSearch(taskId);
    }

    /**
     * 取消全部搜索
     */
    public void abortSearch() {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Aborting search.");
        mSearchAdapter.abortSearch();
    }

    /**
     * 获取预计到达时间
     *
     * @param geoPoint 目标点经纬度
     * @return distance ，travelTime
     */
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(final GeoPoint geoPoint) {
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .build();

        return mSearchAdapter.getTravelTimeFuture(requestParameterBuilder);
    }

    /**
     * 获取两点之间路线距离
     *
     * @param startPoint 起点经纬度
     * @param endPoint 终点经纬度
     * @return distance ，travelTime
     */
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(final GeoPoint startPoint, final GeoPoint endPoint) {
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(startPoint)
                .poiLoc(endPoint)
                .build();

        return mSearchAdapter.getTravelTimeFutureIncludeChargeLeft(requestParameterBuilder);
    }

    /**
     * 更新子点孙节点列表数据
     * @param childInfo 当前子点
     * @return 赋值孙节点数据后的子点
     */
    public CompletableFuture<ChildInfo> setGrandChildInfoList(final ChildInfo childInfo) {
        if (childInfo == null) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "childInfo is null");
            return null;
        }

        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "pid is: " + childInfo.getPoiId());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .poiId(childInfo.getPoiId())
                .queryType(AutoMapConstant.SearchQueryType.ID)
                .searchType(AutoMapConstant.SearchType.POI_SEARCH)
                .userLoc(userLoc)
                .adCode(mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat()))
                .build();
        return mSearchAdapter.setGrandChildInfoList(requestParameterBuilder, childInfo);
    }



    /**
     *
     * @param geoPoint 经纬度点
     * @return ETAInfo 包含距离，剩余时间和剩余电量
     */
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(final GeoPoint geoPoint) {
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .build();
        return mSearchAdapter.getTravelTimeFutureIncludeChargeLeft(requestParameterBuilder);
    }

    /**
     * 添加 POI 点标记
     *
     * @param searchResultEntity SearchResultEntity
     * @param requestParameter SearchRequestParameter
     */
    private void addPoiMarker(final SearchResultEntity searchResultEntity, final SearchRequestParameter requestParameter) {
        if (ConvertUtils.isEmpty(searchResultEntity)) {
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "addPoiMarker: " + searchResultEntity.getSearchType());
        switch (searchResultEntity.getSearchType()) {
            case AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH:
            case AutoMapConstant.SearchType.SEARCH_KEYWORD:
                createPoiMarker(searchResultEntity.getPoiList(), 0, requestParameter);
                break;
            case AutoMapConstant.SearchType.AROUND_SEARCH:
                createPoiMarker(searchResultEntity.getPoiList(), 0);
                createCenterPoiMarker(requestParameter);
                break;
            case AutoMapConstant.SearchType.TERMINAL_PARK_AROUND_SEARCH:
                createTerminalParkPoiMarker(searchResultEntity.getPoiList(), 0);
                createCenterPoiMarker(requestParameter);
                break;
            case AutoMapConstant.SearchType.GEO_SEARCH:
            case AutoMapConstant.SearchType.POI_SEARCH:
                createLabelMarker(searchResultEntity);
                showPreview(searchResultEntity.getPoiList());
                break;
            case AutoMapConstant.SearchType.PID_EN_ROUTE_SEARCH:
                createEnRoutePoiMarker(searchResultEntity.getPoiList());
                showPreview(searchResultEntity.getPoiList());
                break;
            default:
                break;
        }
    }

    /**
     * 创建中心点标记
     * @param searchRequestParameter 搜索请求参数
     */
    public void createCenterPoiMarker(final SearchRequestParameter searchRequestParameter) {
        if (ConvertUtils.isEmpty(searchRequestParameter)) {
            return;
        }
        final LayerItemSearchResult layerItemSearchResult = new LayerItemSearchResult();
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setPoint(searchRequestParameter.getPoiLoc());
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "createCenterPoiMarker: " + searchRequestParameter.getPoiLoc().toString());
        final ArrayList<PoiInfoEntity> poiList = new ArrayList<>();
        poiList.add(poiInfoEntity);
        layerItemSearchResult.setSearchResultPoints(poiList);
        sMarkerInfoMap.put(LayerPointItemType.SEARCH_POI_CENTRAL, layerItemSearchResult);
        mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP,
                LayerPointItemType.SEARCH_POI_CENTRAL, layerItemSearchResult, false);
    }

    /**
     * 搜索结果列表扎标
     * @param poiList 搜索结果列表
     * @param index 当前选中下标
     */
    public void createPoiMarker(final List<PoiInfoEntity> poiList, final int index) {
        if (ConvertUtils.isEmpty(poiList)) {
            return;
        }
        final PoiInfoEntity poiInfoEntity = poiList.get(0);
        final int type = getPointTypeCode(poiInfoEntity.getPointTypeCode());
        final LayerItemSearchResult layerItemSearchResult = new LayerItemSearchResult();
        layerItemSearchResult.setSearchResultPoints((ArrayList<PoiInfoEntity>) poiList);
        final LayerPointItemType layerPointItemType = type == AutoMapConstant.PointTypeCode.CHARGING_STATION ?
                LayerPointItemType.SEARCH_PARENT_CHARGE_STATION :
                LayerPointItemType.SEARCH_PARENT_POINT;
        sMarkerInfoMap.put(layerPointItemType, layerItemSearchResult);
        mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, layerPointItemType,
                layerItemSearchResult, true);
        showPreview(poiList);
    }
    /**
     * 沿途搜结果列表扎标
     * @param poiList 搜索结果列表
     */
    public void createEnRoutePoiMarker(final List<PoiInfoEntity> poiList) {
        if (ConvertUtils.isEmpty(poiList)) {
            return;
        }
        final LayerItemSearchResult layerItemSearchResult = new LayerItemSearchResult();
        layerItemSearchResult.setSearchResultPoints((ArrayList<PoiInfoEntity>) poiList);
        final LayerPointItemType layerPointItemType = LayerPointItemType.SEARCH_POI_ALONG_ROUTE_LIST_SINGLE_POINT;
        sMarkerInfoMap.put(layerPointItemType, layerItemSearchResult);
        mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, layerPointItemType,
                layerItemSearchResult, true);
        showPreview(poiList);
    }


    /**
     * 搜索结果列表扎标
     * @param poiList 搜索结果列表
     * @param index 当前选中下标
     */
    public void updatePoiMarker(final List<PoiInfoEntity> poiList, final int index, final boolean isNeedPreview) {
        if (ConvertUtils.isEmpty(poiList)) {
            return;
        }
        final PoiInfoEntity poiInfoEntity = poiList.get(0);
        final int type = getPointTypeCode(poiInfoEntity.getPointTypeCode());
        final LayerItemSearchResult layerItemSearchResult = new LayerItemSearchResult();
        layerItemSearchResult.setSearchResultPoints((ArrayList<PoiInfoEntity>) poiList);
        final LayerPointItemType layerPointItemType = type == AutoMapConstant.PointTypeCode.CHARGING_STATION ?
                LayerPointItemType.SEARCH_PARENT_CHARGE_STATION :
                LayerPointItemType.SEARCH_PARENT_POINT;
        sMarkerInfoMap.put(layerPointItemType, layerItemSearchResult);
        mLayerAdapter.updateSearchResult(MapType.MAIN_SCREEN_MAIN_MAP, layerPointItemType,
                layerItemSearchResult);
        if (isNeedPreview) {
            showPreview(poiList);
        }
    }

    /**
     * 搜索结果列表扎标
     * @param poiList 搜索结果列表
     * @param index 当前选中下标
     */
    public void createTerminalParkPoiMarker(final List<PoiInfoEntity> poiList, final int index) {
        if (ConvertUtils.isEmpty(poiList)) {
            return;
        }
        final LayerItemSearchResult layerItemSearchResult = new LayerItemSearchResult();
        layerItemSearchResult.setSearchResultPoints((ArrayList<PoiInfoEntity>) poiList);
        final LayerPointItemType layerPointItemType = LayerPointItemType.SEARCH_PARENT_PARK;
        sMarkerInfoMap.put(layerPointItemType, layerItemSearchResult);
        mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, layerPointItemType,
                layerItemSearchResult, false);
        showPreview(poiList);
    }

    /**
     * 移动主图，将搜索扎标移到主图中心点
     * @param poiList 需要扎标的数据列表
     */
    public void showPreview(final List<PoiInfoEntity> poiList) {
        if (ConvertUtils.isEmpty(poiList)) {
            return;
        }
        final List<PreviewParams.PointD> points = poiList.stream()
                .filter(poiInfo -> poiInfo.getPoint() != null)
                .map(poiInfo -> new PreviewParams.PointD(poiInfo.getPoint().getLon(), poiInfo.getPoint().getLat()))
                .collect(Collectors.toList());
        if (!ConvertUtils.isEmpty(poiList) && poiList.size() > 1) {
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "showPointSPreview");
            mMapPackage.showPreview(MapType.MAIN_SCREEN_MAIN_MAP, false, 1350, 210, 600, 140, points, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "showPointPreview");
            if (!ConvertUtils.isEmpty(poiList.get(0).getPoint())) {
                mMapPackage.setMapCenter(MapType.MAIN_SCREEN_MAIN_MAP, poiList.get(0).getPoint());
            }
        }
    }

    /**
     * 移动主图，确保poi边界范围能够被全览
     * @param poiList poi边界点的经纬度列表
     */
    private void showBoundsPreview(final List<GeoPoint> poiList) {
        if (ConvertUtils.isEmpty(poiList)) {
            return;
        }
        final List<PreviewParams.PointD> points = poiList.stream()
                .map(poiInfo -> new PreviewParams.PointD(poiInfo.getLon(), poiInfo.getLat()))
                .collect(Collectors.toList());
        if (!ConvertUtils.isEmpty(poiList) && poiList.size() > 1) {
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "showPointSPreview");
            mMapPackage.showPreview(MapType.MAIN_SCREEN_MAIN_MAP, false, 1350, 210, 600, 140, points, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
        }
    }

    /**
     * 设置高亮扎标
     * @param poiInfoEntity 需要高亮的poi对象
     * @param index 需要高亮的下标
     * @param searchType 扎标对应的搜索类型
     */
    public void setSelectIndex(final PoiInfoEntity poiInfoEntity, final int index, final int searchType) {
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "setSelectIndex type: " + searchType + " ,index: " + index);

        if (searchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) {
            mLayerAdapter.setSearchSelect(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_POI_ALONG_ROUTE
                    , index);
        } else if (searchType == AutoMapConstant.SearchType.TERMINAL_PARK_AROUND_SEARCH) {
            mLayerAdapter.setSearchSelect(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_PARK
                    , index);
            mMapPackage.resetTickCount(MapType.MAIN_SCREEN_MAIN_MAP, 2);
        } else {
            if (getPointTypeCode(poiInfoEntity.getPointTypeCode()) == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
                mLayerAdapter.setSearchSelect(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_CHARGE_STATION
                        , index);
            } else {
                mLayerAdapter.setSearchSelect(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_POINT
                        , index);
            }
        }
        final LayerItemSearchResult layerItemSearchResult = new LayerItemSearchResult();
        final ArrayList<PoiInfoEntity> poiList = new ArrayList<>();
        poiList.add(poiInfoEntity);
        layerItemSearchResult.setSearchResultPoints(poiList);
        //被选中的poi对象需要添加子点，区域，道路的扎标
        if (!ConvertUtils.isEmpty(poiInfoEntity.getMRoadPolygonBounds())) {
            mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_Line_Road,
                    layerItemSearchResult, false);
        }
        if (!ConvertUtils.isEmpty(poiInfoEntity.getMPoiAoiBounds())) {
            mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_AREA,
                    layerItemSearchResult, false);
        }
        if (!ConvertUtils.isEmpty(poiInfoEntity.getChildInfoList())) {
            sMarkerInfoMap.put(LayerPointItemType.SEARCH_CHILD_POINT, layerItemSearchResult);
            mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_CHILD_POINT,
                    layerItemSearchResult, false);
        }
        showPreview(poiList);
    }

    /**
     * 清除高亮
     */
    public void clearFocus(){
        mLayerAdapter.clearFocus(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_POINT);
        mLayerAdapter.clearFocus(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_POI_ALONG_ROUTE);
        mLayerAdapter.clearFocus(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_PARK);
        mLayerAdapter.clearFocus(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_CHARGE_STATION);
    }

    /**
     * 设置子节点高亮下标
     * @param index 高亮下标
     */
    public void setChildIndex(final int index) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "setChildIndex: " + index);
        if(index == -1){
            mLayerAdapter.clearFocus(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_CHILD_POINT);
            return;
        }
        mLayerAdapter.setSearchSelect(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_CHILD_POINT
                , index);
        mMapPackage.resetTickCount(MapType.MAIN_SCREEN_MAIN_MAP, 2);
    }

    /**
     * 搜索结果列表扎标
     * @param poiList 搜索结果列表
     * @param index 当前选中下标
     * @param searchRequestParameter 搜索参数
     */
    private void createPoiMarker(final List<PoiInfoEntity> poiList, final int index, final SearchRequestParameter searchRequestParameter) {
        if (ConvertUtils.isEmpty(poiList)) {
            return;
        }
        final PoiInfoEntity poiInfoEntity = poiList.get(0);
        final int type = getPointTypeCode(poiInfoEntity.getPointTypeCode());
        final LayerItemSearchResult layerItemSearchResult = new LayerItemSearchResult();
        layerItemSearchResult.setSearchResultPoints((ArrayList<PoiInfoEntity>) poiList);
        final LayerPointItemType searchType;
        if (searchRequestParameter.getSearchType() == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) {
            searchType = LayerPointItemType.SEARCH_POI_ALONG_ROUTE;
        } else if (type == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
            searchType = LayerPointItemType.SEARCH_PARENT_CHARGE_STATION;
        } else {
            searchType = LayerPointItemType.SEARCH_PARENT_POINT;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "createPoiMarker searchType " + searchType);
        sMarkerInfoMap.put(searchType, layerItemSearchResult);
        mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, searchType, layerItemSearchResult, true);
        showPreview(poiList);
    }

//    /**
//     * 创建父级点标记
//     *
//     * @param poiInfo PoiInfoEntity
//     * @param index   下标
//     * @return SearchResultLayer
//     */
//    private SearchResultLayer.ParentPoint createParentPoint(final PoiInfoEntity poiInfo, final int index) {
//        final SearchResultLayer.ParentPoint parentPoint = new SearchResultLayer.ParentPoint();
//        parentPoint.id = poiInfo.getPid();
//        parentPoint.poiName = poiInfo.getName();
//        parentPoint.mPos3D = poiInfo.getPoint();
//        parentPoint.index = index;
//        final String typeCode = poiInfo.getPointTypeCode();
//        if (!TextUtils.isEmpty(typeCode)) {
//            parseTypeCodes(typeCode, parentPoint);
//        }
//        return parentPoint;
//    }
//
//    /**
//     * 解析点标记类型
//     *
//     * @param typeCode    poi类型 扎标专用
//     * @param parentPoint ParentPoint
//     */
//    private void parseTypeCodes(final String typeCode, final SearchResultLayer.ParentPoint parentPoint) {
//        for (String code : typeCode.split("\\|")) {
//            if ("0101".startsWith(code)) {
//                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_GAS;
//            }
//            if ("0111".startsWith(code)) {
//                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_CHARGE;
//            }
//            if ("010500".equals(code)) {
//                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_CAR_WASHING;
//            }
//            if ("05".startsWith(code)) {
//                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_FOOD;
//            }
//            if ("1509".startsWith(code)) {
//                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_PARKING;
//            }
//            if ("1803".startsWith(code)) {
//                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_SERVICE_ZONE;
//            }
//            if ("11".startsWith(code)) {
//                parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_SCENIC;
//            }
//            return;
//        }
//        parentPoint.poiType = SearchLayerStyle.INFORMATION_TYPE_UNKONWN;
//    }

    /**
     * label点击扎标
     * @param searchResultEntity SearchResultEntity 数据实体类
     */
    public void createLabelMarker(final SearchResultEntity searchResultEntity) {
        if (ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
            return;
        }
        final PoiInfoEntity firstElement = searchResultEntity.getPoiList().get(0);
        final ArrayList<PoiInfoEntity> list = new ArrayList<>();
        list.add(firstElement);
        final LayerItemSearchResult searchResult = new LayerItemSearchResult();
        searchResult.setSearchResultPoints(list);
        sMarkerInfoMap.put(LayerPointItemType.SEARCH_POI_LABEL, searchResult);
        final boolean addResult = mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP,
                LayerPointItemType.SEARCH_POI_LABEL, searchResult, false);
        //被选中的poi对象需要添加子点，区域，道路的扎标
        if (!ConvertUtils.isEmpty(firstElement.getMRoadPolygonBounds())) {
            mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_Line_Road,
                    searchResult, false);
            final List<GeoPoint> totalList = new ArrayList<>();
            for (List<GeoPoint> tempList : firstElement.getMRoadPolygonBounds()) {
                totalList.addAll(tempList);
            }
            showBoundsPreview(totalList);
        }
        if (!ConvertUtils.isEmpty(firstElement.getMPoiAoiBounds())) {
            mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_PARENT_AREA,
                    searchResult, false);
            final List<GeoPoint> totalList = new ArrayList<>();
            for (List<GeoPoint> tempList : firstElement.getMPoiAoiBounds()) {
                totalList.addAll(tempList);
            }
            showBoundsPreview(totalList);
        }
        if (!ConvertUtils.isEmpty(firstElement.getChildInfoList())) {
            sMarkerInfoMap.put(LayerPointItemType.SEARCH_CHILD_POINT, searchResult);
            mLayerAdapter.updateSearchMarker(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_CHILD_POINT,
                    searchResult, false);
        }
        if (ConvertUtils.isEmpty(firstElement.getMRoadPolygonBounds()) &&
                ConvertUtils.isEmpty(firstElement.getMPoiAoiBounds()) &&
                ConvertUtils.isEmpty(firstElement.getChildInfoList())) {
            //如果没有子点，边界点，道路点，直接以poi点为中心进行展示
            showPreview(searchResultEntity.getPoiList());
        }
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "createLabelMarker result:" + addResult);
    }

    /**
     *清除所有搜索图层的扎标
     */
    public void clearLabelMark() {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "clearLabelMark");
        sMarkerInfoMap.clear();
        mLayerAdapter.clearAllSearchLayerItems(MapType.MAIN_SCREEN_MAIN_MAP);
        if (mNaviPackage.getFixedOverViewStatus()) {
            return;
        }
        mMapPackage.exitPreview(MapType.MAIN_SCREEN_MAIN_MAP, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
    }

    /**
     * 清除点选poi的扎标
     */
    public void clearPoiLabelMark() {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "clearPoiLabelMark");
        mLayerAdapter.clearSearchPOILayerItems(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.SEARCH_POI_LABEL);
        if (mNaviPackage.getFixedOverViewStatus()) {
            return;
        }
        mMapPackage.exitPreview(MapType.MAIN_SCREEN_MAIN_MAP, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
    }

    /**
     * 清除指定类型的扎标
     * @param type 扎标类型
     */
    public void clearTypeMark(final LayerPointItemType type) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "clearTypeMark");
        mLayerAdapter.clearSearchPOILayerItems(MapType.MAIN_SCREEN_MAIN_MAP, type);
    }

    /**
     * 反初始化搜索服务
     */
    public void unInitSearchService() {
        mLayerAdapter.unRegisterLayerClickObserver(MapType.MAIN_SCREEN_MAIN_MAP,  this);
        mSearchAdapter.unInit();
    }

    /**
     * 聚合搜索类型匹配，对应SDK类型 {@link AutoMapConstant.AggregateKeywordType}
     */
    static class AggregateKeywordMatcher {
        private static final Map<Pattern, Integer> KEYWORD_PATTERN_MAP = new LinkedHashMap<>();

        static {
            KEYWORD_PATTERN_MAP.put(Pattern.compile("美食|餐厅"), AutoMapConstant.AggregateKeywordType.DINING);
            KEYWORD_PATTERN_MAP.put(Pattern.compile("景点|旅游"), AutoMapConstant.AggregateKeywordType.SCENIC);
            KEYWORD_PATTERN_MAP.put(Pattern.compile("商圈|购物"), AutoMapConstant.AggregateKeywordType.MALL);
            KEYWORD_PATTERN_MAP.put(Pattern.compile("充电|充电站"), AutoMapConstant.AggregateKeywordType.CHARGING);
            KEYWORD_PATTERN_MAP.put(Pattern.compile("停车|停车场"), AutoMapConstant.AggregateKeywordType.PARKING);
            KEYWORD_PATTERN_MAP.put(Pattern.compile("厕所|卫生间"), AutoMapConstant.AggregateKeywordType.BATHROOM);
        }

        /**
         * 获取聚合搜索类型
         * @param text 聚合搜索类型
         * @return 转换后的Constant类型
         */
        public static int getKeywordType(final String text) {
            if (text == null || text.trim().isEmpty()) {
                return AutoMapConstant.AggregateKeywordType.UNKNOWN;
            }
            for (Map.Entry<Pattern, Integer> entry : KEYWORD_PATTERN_MAP.entrySet()) {
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
        private static final Map<Pattern, Integer> KEYWORD_PATTERN_MAP = new LinkedHashMap<>();

        static {
            KEYWORD_PATTERN_MAP.put(Pattern.compile("服务|服务区"), AutoMapConstant.LineDeepQueryType.SERVICE_AREA);
            KEYWORD_PATTERN_MAP.put(Pattern.compile("加油|加油站"), AutoMapConstant.LineDeepQueryType.GAS_STATION);
            KEYWORD_PATTERN_MAP.put(Pattern.compile("充电|充电站"), AutoMapConstant.LineDeepQueryType.CHARGING);
            KEYWORD_PATTERN_MAP.put(Pattern.compile("停车|停车场"), AutoMapConstant.LineDeepQueryType.PARK_COMMEND);
        }

        /**
         * 获取聚合搜索类型
         * @param text 聚合搜索类型
         * @return 转换后的Constant类型
         */
        public static int getKeywordType(final String text) {
            if (text == null || text.trim().isEmpty()) {
                return AutoMapConstant.LineDeepQueryType.UNKNOWN;
            }
            for (Map.Entry<Pattern, Integer> entry : KEYWORD_PATTERN_MAP.entrySet()) {
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
        final String currentNaviStatus = mNavistatusAdapter.getCurrentNaviStatus();
        final Set<String> validStatuses = new HashSet<>(Set.of(
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
        final GeoPoint point = new GeoPoint();
        point.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        point.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "getCurrentLocation lon:" + point.getLon() + "lat:" + point.getLat());
        return geoSearch(point);
    }

    /**
     * 获取城市代码
     * @return 城市代码
     */
    public int getAcCode() {
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        return mMapDataAdapter.getAdCodeByLonLat(userLoc.getLon(), userLoc.getLat());
    }

    /**
     * 获取当前经纬度
     * @return 经纬度
     */
    public GeoPoint getCurrentLocation() {
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        return userLoc;
    }


    /**
     * 获取POI 类型
     * @param typeCode poi的typeCode
     * @return POI类型
     */
    public int getPointTypeCode(final String typeCode) {
        if (ConvertUtils.isEmpty(typeCode)) {
            return AutoMapConstant.PointTypeCode.OTHERS;
        }
        for (String code : typeCode.split("\\|")) {
            if (code.startsWith("0101")) {
                return AutoMapConstant.PointTypeCode.GAS_STATION;
            }
            if (code.startsWith("0111")) {
                return AutoMapConstant.PointTypeCode.CHARGING_STATION;
            }
            if ("010500".equals(code)) {
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

    /**
     * 搜索结果扎标点击事件回调
     * @param mapTypeId 地图类型
     * @param type 扎标类型
     * @param index 索引
     */
    public void onSearchItemClick(final MapType mapTypeId, final LayerPointItemType type, final int index) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "onSearchItemClick type:" + type);
        final ThreadManager threadManager = ThreadManager.getInstance();
        if (type == LayerPointItemType.SEARCH_CHILD_POINT || type == LayerPointItemType.SEARCH_PARENT_PARK) {
            for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
                final String identifier = entry.getKey();
                mCurrentCallbackId.set(identifier);
                final SearchResultCallback callback = entry.getValue();
                if (type == LayerPointItemType.SEARCH_CHILD_POINT) {
                    threadManager.postUi(() -> callback.onMarkChildClickCallBack(index));
                } else {
                    threadManager.postUi(() -> callback.onMarkTerminalParkClickCallBack(index));
                }
            }
            return;
        }
        LayerItemSearchResult result = null;
        if (!ConvertUtils.isEmpty(sMarkerInfoMap)) {
            result = sMarkerInfoMap.get(type);
            if (result != null && !ConvertUtils.isEmpty(result.getSearchResultPoints())) {
                final List<PoiInfoEntity> list = result.getSearchResultPoints();
                if (!ConvertUtils.isEmpty(list) && index != -1 && index < list.size()) {
                    final PoiInfoEntity info = list.get(index);
                    for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
                        final String identifier = entry.getKey();
                        mCurrentCallbackId.set(identifier);
                        final SearchResultCallback callback = entry.getValue();
                        threadManager.postUi(() -> callback.onMarkClickCallBack(info));
                    }
                }
            }
        }
    }

    /**
     * 计算两点之间的直线距离.
     *
     * @param endPoint 终点.
     * @return 距离.
     */
    public String calcStraightDistance(final GeoPoint endPoint) {
        final GeoPoint startPoint = new GeoPoint();
        startPoint.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        startPoint.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "calcStraightDistance : startPoint = ", startPoint.getLat(),
                startPoint.getLon(), "; endPoint = ", endPoint.getLat(), endPoint.getLon());
        return formatDistanceArrayInternal((int) Math.round(mLayerAdapter.calcStraightDistance(startPoint, endPoint)));
    }

    /**
     * 计算两点之间的直线距离.
     *
     * @param endPoint 终点.
     * @return 距离.
     */
    public int calcStraightDistanceWithInt(final GeoPoint endPoint) {
        final GeoPoint startPoint = new GeoPoint();
        startPoint.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        startPoint.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "calcStraightDistance : startPoint = ", startPoint.getLat(),
                startPoint.getLon(), "; endPoint = ", endPoint.getLat(), endPoint.getLon());
        return (int) Math.round(mLayerAdapter.calcStraightDistance(startPoint, endPoint));
    }

    /**
     * 计算两点之间的直线距离.
     *
     * @param endPoint 终点.
     * @return 距离.
     */
    public String calcStraightDistance(final GeoPoint startPoint,final GeoPoint endPoint) {
        return formatDistanceArrayInternal((int) Math.round(mLayerAdapter.calcStraightDistance(startPoint, endPoint)));
    }

    /**
     * 格式化距离数组.
     * @param distance 原始距离数据
     * @return 解析后的距离文本
     */
    private String formatDistanceArrayInternal(final int distance) {
        final String[] distanceArray = ConvertUtils.formatEnDistanceArray(AppCache.getInstance().getMContext(), distance);
        return distanceArray[0] + distanceArray[1];
    }

    /**
     * 语音poi排序，根据排序规则，将筛选项传递到搜索结果页面.
     *
     * @param mapTypeId MapTypeId。对应地图.
     * @param sortValue String，筛选规则，eg:距离优先、好评优先、低/高价优先.
     */
    public void voiceSortPoi(final MapType mapTypeId, final String sortValue) {
        ThreadManager.getInstance().postUi(() -> {
            for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
                final String identifier = entry.getKey();
                mCurrentCallbackId.set(identifier);
                final SearchResultCallback callback = entry.getValue();
                if (null != callback) {
                    callback.onVoicePoiSort(mapTypeId, sortValue);
                }
            }
        });
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_INPUT)
    private void sentBuryPointForSearch(String keywords){
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, keywords)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * 查询收藏的自营站列表
     * @param idpUserId 用户id
     * @param accessToken token
     * @param vehicleBrand 车型品牌 BUICK, CHEVY, CADILLAC, CADILLAC_IQ
     * @return TaskId
     */
    public int queryCollectStation(String idpUserId,String accessToken,String vehicleBrand){
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .idpUserId(idpUserId)
                .accessToken(accessToken)
                .vehicleBrand(vehicleBrand)
                .build();
        return mSearchAdapter.queryCollectStation(requestParameterBuilder);
    }

    /**
     * 查询自营站列表
     * @param keyword 搜索关键字
     * @return taskId
     */
    public int queryStationNewResult(String keyword){
        final GeoPoint point = new GeoPoint();
        point.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        point.setLat(mPositionAdapter.getLastCarLocation().getLatitude());

        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .keyword(keyword)
                .poiLoc(point)
                .adCode(getAcCode())
                .build();
        return mSearchAdapter.queryStationNewResult(requestParameterBuilder);
    }

    /**
     * 查询云端自营站详情
     * @param poiInfo 充电站信息
     * @return TaskId
     */
    public int queryStationInfo(PoiInfoEntity poiInfo){
        final GeoPoint point = new GeoPoint();
        point.setLon(poiInfo.getPoint().getLon());
        point.setLat(poiInfo.getPoint().getLat());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .poiLoc(point)
                .operatorId(poiInfo.getOperatorId())
                .stationId(poiInfo.getStationId())
                .build();
        return mSearchAdapter.queryStationInfo(requestParameterBuilder);
    }

    /**
     * 查询充电桩详情信息
     * @param equipmentId 充电桩id
     * @param operatorId 运营商id
     * @param stationId 充电站id
     * @return taskId
     */
    public int queryEquipmentInfo(String equipmentId, String operatorId, String stationId){
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .operatorId(operatorId)
                .stationId(stationId)
                .equipmentId(equipmentId)
                .build();
        return mSearchAdapter.queryEquipmentInfo(requestParameterBuilder);
    }

    /**
     * 查询充电桩详情信息
     * @param info 预约单信息
     * @return taskId
     */
    public int queryEquipmentInfo(ReservationInfo info){
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .operatorId(info.getmOperatorId())
                .stationId(info.getmStationId())
                .equipmentId(info.getmEquipmentId())
                .build();
        return mSearchAdapter.queryEquipmentInfo(requestParameterBuilder);
    }

    /**
     * 搜索结果列表页面可变状态变化回调
     * @param isShow 是否可见
     */
    public void updateShowState(final boolean isShow) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "updateShowState : " + isShow);
        mIsShow = isShow;
        for (Map.Entry<String, SearchResultCallback> entry : mISearchResultCallbackMap.entrySet()) {
            final String identifier = entry.getKey();
            mCurrentCallbackId.set(identifier);
            final SearchResultCallback callback = entry.getValue();
            callback.onShowStateChanged(isShow);
        }
    }

    /**
     * 创建公桩预约
     * @param item 充电桩接口
     * @param poiInfo 充电站信息
     * @param idpUserId 用户id
     * @param accessToken token
     * @param vehicleBrand 1.别克 2.凯迪 3.雪佛兰 4.奥特能 5.泛亚
     * @return taskId
     */
    public int createReservation(ConnectorInfoItem item, PoiInfoEntity poiInfo,String idpUserId,String accessToken,String vehicleBrand){
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .operatorId(poiInfo.getOperatorId())
                .stationId(poiInfo.getStationId())
                .connectorId(item.getmConnectorId())
                .source("CARAPP")
                .vehicleBrand(vehicleBrand)
                .idpUserId(idpUserId)
                .accessToken(accessToken)
                .build();
        return mSearchAdapter.createReservation(requestParameterBuilder);
    }

    /**
     * 解开地锁
     * @param item 充电桩接口
     * @param poiInfo 充电站信息
     * @param idpUserId 用户id
     * @param accessToken token
     * @param vehicleBrand 1.别克 2.凯迪 3.雪佛兰 4.奥特能 5.泛亚
     * @return taskId
     */
    public int unGroundLock(ConnectorInfoItem item, PoiInfoEntity poiInfo,String idpUserId,String accessToken,String vehicleBrand){
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .accessToken(accessToken)
                .operatorId(poiInfo.getOperatorId())
                .vehicleBrand(vehicleBrand)
                .idpUserId(idpUserId)
                .connectorId(item.getmConnectorId())
                .source("CARAPP")
                .build();
        return mSearchAdapter.unGroundLock(requestParameterBuilder);
    }

    /**
     * 修改收藏状态
     * @param idpUserId 用户id
     * @param accessToken token
     * @param vehicleBrand BUICK, CHEVY, CADILLAC, CADILLAC_IQ
     * @param poiInfo 充电站信息
     * @return taskId
     */
    public int updateCollectStatus(String idpUserId,String accessToken,String vehicleBrand,PoiInfoEntity poiInfo){
        ArrayList<SavedStations> list = new ArrayList<>();
        SavedStations savedStations = new SavedStations()
                .setmStationSaved(!poiInfo.getIsCollect())
                .setmLastUpdateTime(System.currentTimeMillis())
                .setmStationId(poiInfo.getStationId())
                .setmOperatorId(poiInfo.getOperatorId());
        list.add(savedStations);
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .idpUserId(idpUserId)
                .accessToken(accessToken)
                .vehicleBrand(vehicleBrand)
                .channel("CSM")
                .savedStationsJson(list)
                .build();
        return mSearchAdapter.updateCollectStatus(requestParameterBuilder);
    }

    /**
     * 查询当前用户的预约单列表
     * @param brandId 1.凯迪 2.别克 3.雪佛兰 4.奥特能
     * @param status 1：已预约 2：已完成 3：已取
     * @param idpUserId 用户id
     * @param accessToken token
     * @return taskId
     */
    public int queryReservation(String brandId,int status,String idpUserId,String accessToken){
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .vehicleBrand(brandId)
                .type(status)
                .idpUserId(idpUserId)
                .accessToken(accessToken)
                .build();
        return mSearchAdapter.queryReservation(requestParameterBuilder);
    }

    /**
     * 根据预约单号查询当前用户的预约单列表
     * @param preNum 预约单号
     * @param brandId 1.凯迪 2.别克 3.雪佛兰 4.奥特能
     * @param idpUserId 用户id
     * @param accessToken token
     * @return taskId
     */
    public int queryReservationByPreNum(String preNum,String brandId,String idpUserId,String accessToken){
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .vehicleBrand(brandId)
                .preNum(preNum)
                .idpUserId(idpUserId)
                .accessToken(accessToken)
                .build();
        return mSearchAdapter.queryReservation(requestParameterBuilder);
    }

    /**
     * 取消预约
     * @param preNum 预约单号
     * @param idpUserId 用户id
     * @param accessToken token
     * @return taskId
     */
    public int cancelReservation(String preNum,String idpUserId,String accessToken){
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .preNum(preNum)
                .idpUserId(idpUserId)
                .accessToken(accessToken)
                .build();
        return mSearchAdapter.cancelReservation(requestParameterBuilder);

    }

    /**
     * 映射云端侧对应的brandId
     * @param brandId 1 Buick 2 Cadillac 3 Chevrolet
     * @return
     */
    public String getBrandId(int brandId){
        return switch (brandId){
            case 1 -> "2";
            case 2 -> "1";
            case 3 -> "3";
            default -> "4";
        };
    }

    /**
     * 映射云端侧对应的brandName
     * @param brandId 1 Buick 2 Cadillac 3 Chevrolet
     * @return
     */
    public String getBrandName(int brandId){
        return switch (brandId){
            case 1 -> "BUICK";
            case 2 -> "CADILLAC";
            case 3 -> "CHEVY";
            default -> "";
        };
    }
    /**
     * 判断是否登录
     * @return 是否登录
     */
    public boolean isLogin() {
        final AccountProfileInfo info;
        final String valueJson = mCommonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        if (!TextUtils.isEmpty(valueJson)) {
            info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            if (info != null) {
                return !TextUtils.isEmpty(info.getUid());
            }
        }
        return false;
    }

    /**
     * 创建预约倒计时，30分钟内需提醒用户充电
     * @param destPoint 充电桩的坐标信息
     */
    public void createTimeTick(GeoPoint destPoint){
        cancelTimeTick();
        Long lastTime = TimeUtils.getInstance().getCurrentMillSeconds();
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(new Runnable() {
            @Override
            public void run() {
                final GeoPoint currentPoint = new GeoPoint();
                currentPoint.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
                currentPoint.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
                getTravelTimeFutureIncludeChargeLeft(destPoint,currentPoint).thenAccept(etaInfo -> {
                    mSearchAdapter.calcTip(lastTime,etaInfo.getTime());
                }).exceptionally(throwable -> {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG,"query travel time error");
                    return null;
                });
            }
        },0,60, TimeUnit.SECONDS);
    }

    /**
     * 取消预约倒计时
     */
    public void cancelTimeTick(){
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    /**
     * 设置预约单号
     * @param preNum 预约单号
     */
    public void setReservationPreNum(String preNum){
        mReservationPreNum = preNum;
    }

    /**
     * 获取预约单号，可以根据预约单号进行预约单信息查询
     * @return
     */
    public String getReservationPreNum(){
        if(ConvertUtils.isNull(mReservationPreNum)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"get mReservationInfo is null");
            return "";
        }
        return mReservationPreNum;
    }

    public long getRestAreaSapaDetail(final String pid){
        PathInfo currentPathInfo = OpenApiHelper.getCurrentPathInfo(MapType.MAIN_SCREEN_MAIN_MAP);
        long sapaDetail = 0;
        if(!ConvertUtils.isEmpty(currentPathInfo)){
            ArrayList<RestAreaInfo> restAreaInfo = currentPathInfo.getRestAreas(0,255);
            ArrayList<RestAreaInfo> restFilterAreaInfos = new ArrayList<>();

            if(!ConvertUtils.isEmpty(restAreaInfo)){
                restFilterAreaInfos = restAreaInfo.stream().filter(v -> pid.equals(v.servicePOIID)).collect(Collectors.toCollection(ArrayList::new));
            }
            if(!ConvertUtils.isEmpty(restFilterAreaInfos)){
                sapaDetail = restFilterAreaInfos.get(0).sapaDetail;
            }
        }
        return sapaDetail;
    }
}