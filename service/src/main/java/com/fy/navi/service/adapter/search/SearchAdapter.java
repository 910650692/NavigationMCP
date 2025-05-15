package com.fy.navi.service.adapter.search;

import android.util.Pair;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.SearchRequestParameter;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

/**
 * @author baipeng0904
 * SearchAdapter类提供了一种方式来执行各种搜索操作和管理搜索回调.
 * 封装了对具体搜索API的调用.
 * @version \$Revision1.0\$
 */
final public class SearchAdapter {
    // 搜索API包名，用于动态加载实现类
    private static final String SEARCH_API_PKG = Objects.requireNonNull(SearchAdapter.class.getPackage()).getName();
    // 搜索API类名，用于动态加载实现类
    private static final String SEARCH_API_CLS = "SearchAdapterImpl";
    // ISearchApi接口的实现，用于执行搜索操作
    private final ISearchApi mSearchApi;

    private SignalPackage mSignalPackage;
    private CalibrationPackage mCalibrationPackage;

    /**
     * 私有构造方法，防止外部实例化.
     * 初始化mSearchApi，通过AdapterConfig动态加载指定的搜索API实现类.
     */
    private SearchAdapter() {
        mSearchApi = (ISearchApi) AdapterConfig.getObject(SEARCH_API_PKG, SEARCH_API_CLS);
        mSignalPackage = SignalPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
    }

    /**
     * SearchAdapterHolder内部类用于实现单例模式的懒加载.
     */
    final private static class SearchAdapterHolder {
        // 单例实例
        private static final SearchAdapter INSTANCE = new SearchAdapter();
    }

    /**
     * 获取SearchAdapter的单例实例.
     *
     * @return SearchAdapter的实例
     */
    public static SearchAdapter getInstance() {
        return SearchAdapterHolder.INSTANCE;
    }

    /**
     * 注册搜索结果回调.
     *
     * @param callback 搜索结果回调接口实现
     */
    public void registerCallBack(final ISearchResultCallback callback) {
        mSearchApi.registerSearchObserver(callback);
    }

    /**
     * 反注册搜索结果回调.
     *
     * @param callback 搜索结果回调接口实现
     */
    public void unRegisterSearchObserver(final ISearchResultCallback callback) {
        mSearchApi.unRegisterSearchObserver(callback);
    }

    /**
     * 初始化搜索API.
     */
    public void init() {
        mSearchApi.init();
    }

    /**
     * 解注册搜索API.
     */
    public void unInit() {
        mSearchApi.unInitSearchApi();
    }

    /**
     * 执行预搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int suggestionSearch(final SearchRequestParameter parameterBuilder) {
        return mSearchApi.suggestionSearch(parameterBuilder);
    }

    /**
     * 执行POI详情搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int poiDetailSearch(final SearchRequestParameter parameterBuilder) {
        return mSearchApi.poiDetailSearch(parameterBuilder);
    }

    /**
     * 根据POI ID执行搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int poiIdSearch(final SearchRequestParameter parameterBuilder) {
        return mSearchApi.poiIdSearch(parameterBuilder);
    }

    /**
     * 根据经纬度搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int geoSearch(final SearchRequestParameter parameterBuilder) {
        return mSearchApi.geoSearch(parameterBuilder);
    }

    /**
     * 深度信息搜索.
     *
     * @param parameter 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int deppInfoSearch(final SearchRequestParameter parameter) {
        return mSearchApi.deppInfoSearch(parameter);
    }

    /**
     * 沿途批量深度信息搜索
     *
     * @param requestParameter 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int doLineDeepInfoSearch(final SearchRequestParameter requestParameter) {
        return mSearchApi.doLineDeepInfoSearch(requestParameter);
    }

    /**
     * 执行关键字搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskID
     */
    public int keywordSearch(final SearchRequestParameter parameterBuilder) {
        return mSearchApi.keyWordSearch(parameterBuilder);
    }

    /**
     * 执行 沿途搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int alongWaySearch(final SearchRequestParameter parameterBuilder) {
        return mSearchApi.alongWaySearch(parameterBuilder);
    }

    /**
     * 执行周边搜索.
     *
     * @param parameterBuilder 参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int aroundSearch(final SearchRequestParameter parameterBuilder) {
        return mSearchApi.aroundSearch(parameterBuilder);
    }

    /**
     * V2聚合搜索
     *
     * @param searchRequestParameter 参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int aggregateSearch(final SearchRequestParameter searchRequestParameter) {
        return mSearchApi.aggregateSearch(searchRequestParameter);
    }

    /**
     * V2顺路搜索
     *
     * @param searchRequestParameter 参数{@link SearchRequestParameter}
     * @return taskId
     */
    public int enRouteKeywordSearch(final SearchRequestParameter searchRequestParameter) {
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
    public void abortSearch(final int taskId) {
        mSearchApi.abortSearch(taskId);
    }

    /**
     * 获取预计到达时间
     *
     * @param searchRequestParameter SearchRequestParameter
     * @return distance ，travelTime
     */
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(final SearchRequestParameter searchRequestParameter) {
        return mSearchApi.getTravelTimeFuture(searchRequestParameter);
    }

    /**
     * 获取预计到达时间,剩余距离,剩余电量
     *
     * @param searchRequestParameter SearchRequestParameter
     * @return ETAInfo
     */
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(final SearchRequestParameter searchRequestParameter) {
        if (mSignalPackage.getBatteryEnergy() >= 0) {
            BevPowerCarUtils.getInstance().initlialHVBattenergy = mSignalPackage.getBatteryEnergy();
        }
        if (mSignalPackage.getMaxBatteryEnergy() > 0) {
            BevPowerCarUtils.getInstance().maxBattenergy = mSignalPackage.getMaxBatteryEnergy();
        }
        if (mCalibrationPackage.highVoltageBatteryPropulsionTotalRangeNavi() > 0) {
            BevPowerCarUtils.getInstance().batterToDistanceCarSignal = (double)
                    mCalibrationPackage.highVoltageBatteryPropulsionTotalRangeNavi() / 100;
        }
        return mSearchApi.getTravelTimeFutureIncludeChargeLeft(searchRequestParameter);
    }

    // 查询收藏充电站
    public int queryCollectStation(final SearchRequestParameter searchRequestParameter){
        return mSearchApi.queryCollectStation(searchRequestParameter);
    }

    // 查询自营站列表
    public int queryStationNewResult(final SearchRequestParameter searchRequestParameter){
        return mSearchApi.queryStationNewResult(searchRequestParameter);
    }

    // 查询自营站信息
    public int queryStationInfo(final SearchRequestParameter searchRequestParameter){
        return mSearchApi.queryStationInfo(searchRequestParameter);
    }

    // 查询充电桩信息
    public int queryEquipmentInfo(final SearchRequestParameter searchRequestParameter){
        return mSearchApi.queryEquipmentInfo(searchRequestParameter);
    }

    // 创建预约
    public int createReservation(final SearchRequestParameter searchRequestParameter){
        return mSearchApi.createReservation(searchRequestParameter);
    }

    // 解开地锁
    public int unGroundLock(final SearchRequestParameter searchRequestParameter){
        return mSearchApi.unGroundLock(searchRequestParameter);
    }

    public int updateCollectStatus(final SearchRequestParameter searchRequestParameter){
        return mSearchApi.updateCollectStatus(searchRequestParameter);
    }
}
