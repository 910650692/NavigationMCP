package com.fy.navi.hmi.route.alternative;

import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.route.AlterChargeViewModel;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;


import java.util.concurrent.CompletableFuture;

public class AlterChargeModel extends BaseModel<AlterChargeViewModel> implements IRouteResultObserver, SearchResultCallback, ILayerPackageCallBack {
    private static final String TAG = "AlterChargeModel";
    private final RoutePackage mRoutePackage;
    private final SearchPackage mSearchPackage;
    private final LayerPackage mLayerPackage;
    private long mAlterChargeStationTaskId;
    private int mSearchTaskId = -1;
    private int mCurrentTaskId = -1;

    public AlterChargeModel() {
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
    }

    @Override
    public void onCreate() {
        mRoutePackage.registerRouteObserver(TAG, this);
        mSearchPackage.registerCallBack(TAG, this);
        mLayerPackage.registerCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mRoutePackage.unRegisterRouteObserver(TAG);
        mSearchPackage.unRegisterCallBack(TAG);
        mLayerPackage.unRegisterCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
    }

    /**
     * 请求替换充电站信息
     * @param poiId poiID
     */
    public void requestAlterChargeStation(final String poiId) {
        mAlterChargeStationTaskId = mRoutePackage.requestRouteAlternativeChargeStation(MapType.MAIN_SCREEN_MAIN_MAP, poiId);
    }

    /**
     * 请求充电站详情信息
     * @param poiId poiID
     */
    public void getSearchDetailsMode(final String poiId) {
        mSearchTaskId = mSearchPackage.poiIdSearch(poiId);
    }

    /**
     * 请求当前充电站详情信息
     * @param poiId poiID
     */
    public void getCurrentDetails(final String poiId) {
        mCurrentTaskId = mSearchPackage.poiIdSearch(poiId);
    }

    /**
     * 获取预计到达时间
     * @param geoPoint 目前坐标
     * @return 返回到达时间
     */
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(final GeoPoint geoPoint) {
        return mSearchPackage.getTravelTimeFuture(geoPoint);
    }

    /**
     * 获取预计到达时间与预计电量
     * @param geoPoint 目前坐标
     * @return 返回到达时间
     */
    public CompletableFuture<ETAInfo> getTravelTimeFutureWithEv(final GeoPoint geoPoint) {
        return mSearchPackage.getTravelTimeFutureIncludeChargeLeft(geoPoint);
    }

    /**
     * 添加途径点
     * @param info 替换充电站信息
     */
    public void addViaList(final RouteAlterChargeStationInfo info) {
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        final GeoPoint geoPoint = new GeoPoint(info.getMPos().getLon(), info.getMPos().getLat(), info.getMPos().getZ());
        poiInfoEntity.setPid(info.getMPoiId());
        poiInfoEntity.setName(info.getMName());
        poiInfoEntity.setTypeCode("011100");
        poiInfoEntity.setPoint(geoPoint);

        mRoutePackage.addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
        mViewModel.getClosePage().call();
    }

    /**
     * 添加途径点
     * @param poiInfoEntities 点信息
     */
    public void addViaList(final PoiInfoEntity poiInfoEntities) {
        mRoutePackage.addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntities);
        mViewModel.getClosePage().call();
    }

    /**
     * 清除图层备选充电站扎标
     */
    public void clearLayerItem() {
        mRoutePackage.clearRouteItemByType(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE);
    }

    @Override
    public void onRouteAlterChargeStationInfo(final RouteAlterChargeStationParam routeAlterChargeStationParam) {
        if (mAlterChargeStationTaskId == routeAlterChargeStationParam.getMRequestId()) {
            mViewModel.showAlterChargeStationInfo(routeAlterChargeStationParam);
        }
    }

    /**
     * 正常搜索结果回调方法
     * 在完成搜索操作后被调用，用于返回搜索结果给 HMI处理界面
     *
     * @param taskId             taskId,请求的唯一标识
     * @param errorCode          错误码，表示搜索操作的结果状态
     * @param message            错误消息，描述搜索操作的结果信息
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     */
    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        Logger.d(TAG, "onSearchResult");
        if (searchResultEntity == null) {
            Logger.d(TAG, "searchResultEntity is null");
            return;
        }
        if (mSearchTaskId == taskId) {
            Logger.i(TAG, GsonUtils.toJson(searchResultEntity.getPoiList()));
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH ||
                    searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH) {
                final PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
                if (!ConvertUtils.isEmpty(poiInfoEntity)) {
                    mViewModel.showChargeStationDetail(poiInfoEntity);
                }
            }
        } else if (mCurrentTaskId == taskId) {
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH ||
                    searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH) {
                if (ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                    return;
                }
                final PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
                if (!ConvertUtils.isEmpty(poiInfoEntity)) {
                    mViewModel.showCurrentChargeStation(poiInfoEntity);
                }
            }
        }
    }

    @Override
    public void onRouteItemClick(final MapType mapTypeId, final LayerPointItemType type, final LayerItemRoutePointClickResult result) {
        Logger.d(TAG, "onRouteItemClick");
        if (ConvertUtils.isEmpty(result) || ConvertUtils.isEmpty(type)) {
            return;
        }
        if (!NaviStatus.NaviStatusType.SELECT_ROUTE.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            Logger.i(TAG, "is not on route page");
            return;
        }
        switch (type) {
            case ROUTE_POINT_VIA_REPLACE_CHARGE:
                mViewModel.getSearchDetailsMode((int) result.getIndex());
                break;
            case ROUTE_POINT_WEATHER:
            case ROUTE_POINT_REST_AREA:
                mViewModel.getClosePage().call();
                break;
            default:
                break;
        }
    }
}
