package com.sgm.navi.hmi.route.newAlterCharge;

import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.route.NewAlterChargeViewModel;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteAlterChargeStationParam;
import com.sgm.navi.service.define.search.ETAInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

public class NewAlterChargeModel extends BaseModel<NewAlterChargeViewModel> implements IRouteResultObserver, SearchResultCallback, ILayerPackageCallBack {
    private static final String TAG = "AlterChargeModel";
    private final RoutePackage mRoutePackage;
    private final SearchPackage mSearchPackage;
    private final LayerPackage mLayerPackage;
    private ConcurrentHashMap<Long, Integer> mAlterChargeStationTaskId = new ConcurrentHashMap<>();
    private int mCurrentTaskId = -1;
    private int mSupplementListTaskId = -1;
    private List<String> mSupplementPidList = new ArrayList<>();
    private int mAlterListTaskId = -1;
    private List<String> mAlterPidList = new ArrayList<>();

    public NewAlterChargeModel() {
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
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
    public void requestAlterChargeStation(final String poiId, final int index) {
        long taskId = mRoutePackage.requestRouteAlternativeChargeStation(MapType.MAIN_SCREEN_MAIN_MAP, poiId);
        if (taskId != -1) {
            mAlterChargeStationTaskId.put(taskId, index);
        }
    }

    /**
     * 请求替换充电站信息
     */
    public void clearAlterChargeStation() {
        mAlterChargeStationTaskId.clear();
    }

    /**
     * 批量搜索详情信息
     * @param pidList poiID列表
     */
    public void getPoiListSearch(final List<String> pidList) {
        mSupplementPidList.clear();
        mSupplementPidList.addAll(pidList);
        mSupplementListTaskId = mSearchPackage.poiListSearch(pidList, 2, true);
    }

    /**
     * 批量搜索替换详情信息
     * @param pidList poiID列表
     */
    public void getAlterPoiListSearch(final List<String> pidList) {
        mAlterPidList.clear();
        mAlterPidList.addAll(pidList);
        mAlterListTaskId = mSearchPackage.poiListSearch(pidList, 2, true);
    }

    /**
     * 请求当前充电站详情信息
     * @param poiId poiID
     */
    public void getCurrentDetails(final String poiId) {
        mCurrentTaskId = mSearchPackage.poiIdSearch(poiId, true);
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
     * 替换补能点
     * @param newPoiInfoEntity 替换点信息
     * @param oldPoiInfoEntity 被替换点信息
     */
    public void replaceSupplement(final PoiInfoEntity newPoiInfoEntity, final PoiInfoEntity oldPoiInfoEntity) {
        mRoutePackage.replaceSupplement(MapType.MAIN_SCREEN_MAIN_MAP, newPoiInfoEntity, oldPoiInfoEntity);
        mViewModel.getClosePage().call();
    }

    /**
     * 清除图层备选充电站扎标
     */
    public void clearLayerItem() {
        Logger.d(TAG, "clearLayerItem");
        mRoutePackage.clearRouteItemByType(MapType.MAIN_SCREEN_MAIN_MAP, LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE);
        MapPackage.getInstance().resetTickCount(MapType.MAIN_SCREEN_MAIN_MAP,2);
    }

    public void updateRouteReplaceChargePoints(final ArrayList<RouteAlterChargeStationInfo> RouteAlterChargeStationInfos) {
        Logger.d(TAG, "updateRouteReplaceChargePoints : " + RouteAlterChargeStationInfos.size());
        mRoutePackage.updateRouteReplaceChargePoints(MapType.MAIN_SCREEN_MAIN_MAP, RouteAlterChargeStationInfos);
    }

    @Override
    public void onRouteAlterChargeStationInfo(final RouteAlterChargeStationParam routeAlterChargeStationParam) {
        Integer index = mAlterChargeStationTaskId.get(routeAlterChargeStationParam.getMRequestId());
        if (index != null) {
            mViewModel.showAlterChargeStationInfo(routeAlterChargeStationParam, index);
        }
    }

    @Override
    public void onSilentSearchResult(final int taskId, final int errorCode,
                                     final String message, final SearchResultEntity searchResultEntity) {
        Logger.d(TAG, "onSearchResult");
        if (searchResultEntity == null) {
            Logger.d(TAG, "searchResultEntity is null");
            return;
        }
        if (mCurrentTaskId == taskId) {
            if (Logger.openLog) {
                Logger.i(TAG, searchResultEntity.getPoiList());
            }
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH ||
                    searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH) {
                final ArrayList<PoiInfoEntity> poiInfoEntities = (ArrayList<PoiInfoEntity>) searchResultEntity.getPoiList();
                if (poiInfoEntities != null && !poiInfoEntities.isEmpty()) {
                    final PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
                    if (!ConvertUtils.isEmpty(poiInfoEntity) && mViewModel != null) {
                        mViewModel.showChargeStationDetail(poiInfoEntity);
                    }
                }
            }
        } else if (mSupplementListTaskId == taskId) {
            final ArrayList<PoiInfoEntity> poiInfoEntities = (ArrayList<PoiInfoEntity>) searchResultEntity.getPoiList();
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.PID_LIST_SEARCH
                    && poiInfoEntities != null && !poiInfoEntities.isEmpty()) {
                if (mSupplementPidList == null || mSupplementPidList.isEmpty() || mSupplementPidList.size() != poiInfoEntities.size()) {
                    Logger.d(TAG, "data exception");
                    return;
                }
                if (mViewModel != null) {
                    mViewModel.setSilentSearchResult(pidSorting(poiInfoEntities, mSupplementPidList));
                }
            }
        } else if (mAlterListTaskId == taskId) {
            final ArrayList<PoiInfoEntity> poiInfoEntities = (ArrayList<PoiInfoEntity>) searchResultEntity.getPoiList();
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.PID_LIST_SEARCH
                    && poiInfoEntities != null && !poiInfoEntities.isEmpty()) {
                if (mAlterPidList == null || mAlterPidList.isEmpty() || mAlterPidList.size() != poiInfoEntities.size()) {
                    Logger.d(TAG, "data exception");
                    return;
                }
                if (mViewModel != null) {
                    mViewModel.setAlterSilentSearchResult(pidSorting(poiInfoEntities, mAlterPidList));
                }
            }
        }
    }

    @Override
    public void onRouteSlected(final MapType mapTypeId, final int routeIndex, final boolean isFirst) {
        mViewModel.getClosePage().call();
    }


    public ArrayList<PoiInfoEntity> pidSorting(final ArrayList<PoiInfoEntity> poiInfoEntities, final List<String> pidList) {
        ArrayList<PoiInfoEntity> sortedList = new ArrayList<>();

        Map<String, PoiInfoEntity> poiMap = new HashMap<>();
        for (PoiInfoEntity entity : poiInfoEntities) {
            poiMap.put(entity.getPid(), entity);
        }

        for (String pid : pidList) {
            PoiInfoEntity entity = poiMap.get(pid);
            if (entity != null) {
                sortedList.add(entity);
            } else {
                Logger.d(TAG, "pidSorting: " + pid);
                sortedList.add(new PoiInfoEntity());
            }
        }
        return sortedList;
    }
}
