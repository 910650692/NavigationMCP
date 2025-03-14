package com.fy.navi.hmi.route.alternative;

import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.route.AlterChargeViewModel;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;


import java.util.concurrent.CompletableFuture;

public class AlterChargeModel extends BaseModel<AlterChargeViewModel> implements IRouteResultObserver, SearchResultCallback {
    private static final String TAG = "AlterChargeModel";
    private static final int ALTER_CHARGE_DETAIL = 1;
    private RoutePackage routePackage;
    private SearchPackage searchPackage;
    private long alterChargeStationTaskId;
    private int searchTaskId = -1;
    private int listSearchType;

    public AlterChargeModel() {
        routePackage = RoutePackage.getInstance();
        searchPackage = SearchPackage.getInstance();
        routePackage.registerRouteObserver(TAG, this);
        searchPackage.registerCallBack(TAG, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        routePackage.unRegisterRouteObserver(TAG);
        searchPackage.unRegisterCallBack(TAG);
    }

    public void requestAlterChargeStation(String poiID) {
        alterChargeStationTaskId = routePackage.requestRouteAlternativeChargeStation(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiID);
    }

    public void getSearchDetailsMode(String poiId) {
        listSearchType = ALTER_CHARGE_DETAIL;
        searchTaskId = searchPackage.poiIdSearch(poiId);
    }

    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(GeoPoint geoPoint) {
        return searchPackage.getTravelTimeFuture(geoPoint);
    }

    public void addViaList(RouteAlterChargeStationInfo info) {
        PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        GeoPoint geoPoint = new GeoPoint(info.pos.lon, info.pos.lat, info.pos.z);
        poiInfoEntity.setPid(info.poiId);
        poiInfoEntity.setName(info.name);
        poiInfoEntity.setTypeCode("011100");
        poiInfoEntity.setPoint(geoPoint);

        routePackage.addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
        mViewModel.closePage.call();
    }

    public void addViaList(PoiInfoEntity poiInfoEntities) {
        routePackage.addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntities, RoutePoiType.ROUTE_POI_TYPE_WAY);
        mViewModel.closePage.call();
    }

    @Override
    public void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {
        if (alterChargeStationTaskId == routeAlterChargeStationParam.getRequestId()) {
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
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (searchTaskId != taskId) return;
        Logger.d(TAG, "onSearchResult");
        Logger.i(TAG, GsonUtils.toJson(searchResultEntity.getPoiList()));
        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH) {
            PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
            if (!ConvertUtils.isEmpty(poiInfoEntity)) {
                if (listSearchType == ALTER_CHARGE_DETAIL) {
                    mViewModel.showChargeStationDetail(poiInfoEntity);
                }

            }
        }
    }
}
