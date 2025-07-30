package com.sgm.navi.hmi.search.searchresult;


import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.MapTypeManager;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.ui.base.BaseModel;

import java.util.UUID;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
public class SearchResultModel extends BaseModel<SearchResultViewModel> implements SearchResultCallback, ILayerPackageCallBack {
    private static final String TAG = "SearchResultModel";
    private final SearchPackage mSearchPackage;
    private final LayerPackage mLayerPackage;
    private final String mCallbackId;
    private final RoutePackage mRoutePackage;
    private IRouteResultObserver mRouteResultObserver;
    private int mTaskId;
    private SearchResultEntity mSearchResultEntity;

    public SearchResultModel() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "SearchResultModel 初始化" + this);
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        initRouteCallback();
    }

    /**
     * 注册路线变化回调
     */
    public void registerRouteCallback() {
        mRoutePackage.registerRouteObserver(mCallbackId, mRouteResultObserver);
    }

    /**
     * 初始化路线规划接口回调.
     */
    private void initRouteCallback() {
        mRouteResultObserver = new IRouteResultObserver() {

            @Override
            public void onRouteSlected(MapType mapTypeId, int routeIndex, boolean isFirst) {
                mViewModel.onRouteSelected();
            }
        };
    }

    /**
     * 日夜模式切换保存数据到model
     * @param taskId 请求数据的taskId
     * @param searchResultEntity SearchResultEntity对象
     */
    public void saveData(final int taskId, final SearchResultEntity searchResultEntity) {
        this.mTaskId = taskId;
        this.mSearchResultEntity = searchResultEntity;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mSearchPackage.registerCallBack(mCallbackId, this);
    }

    /**
     * 恢复fragment状态
     */
    public void onReStoreFragment() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onReStoreFragment: " + mViewModel + " ,taskId: " + mTaskId);
        final ThreadManager threadManager = ThreadManager.getInstance();
        threadManager.postUi(() -> {
            if (mSearchResultEntity != null) {
                mSearchPackage.createPoiMarker(mSearchResultEntity.getPoiList(), 0);
            }
            mViewModel.notifySearchResult(mTaskId, mSearchResultEntity);
        });
    }

    /**
     * 搜索结果列表页面可变状态变化回调
     * @param isShow 是否可见
     */
    public void updateShowState(final boolean isShow) {
        mSearchPackage.updateShowState(isShow);
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH) {
                return;
            }
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) {
                final ThreadManager threadManager = ThreadManager.getInstance();
                mTaskId = taskId;
                mSearchResultEntity = searchResultEntity;
                threadManager.postUi(() -> mViewModel.notifySearchResult(taskId, searchResultEntity));
            }
        }
    }

    @Override
    public void onSilentSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH) {
                return;
            }
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) {
                mViewModel.notifySilentSearchResult(taskId, searchResultEntity);
            }
        }
    }

    @Override
    public void onSearchItemClick(MapType mapTypeId, LayerPointItemType type, int index) {
        Logger.d(TAG, "onSearchItemClick : " + type);
        if (type == LayerPointItemType.SEARCH_POI_ALONG_ROUTE_ADD) {
            if (mViewModel != null) {
                mViewModel.onSearchItemClick(index);
            }
        }
    }

    @Override
    public void onVoicePoiSort(final MapType mapTypeId, final String sortValue) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            Logger.d(TAG, "onVoicePoiSort: " + sortValue);
            mViewModel.onVoicePoiSort(sortValue);
        }
    }

    @Override
    public void onVoicePoiSort(final MapType mapTypeId, final String sortValue, final GeoPoint point) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            Logger.d(TAG, "onVoicePoiSort: " + sortValue + " ,point: " + point);
            mViewModel.onVoicePoiSort(sortValue, point);
        }
    }

    @Override
    public void onStart() {
        super.onStart();
        if (mLayerPackage != null) {
            Logger.d(TAG, "registerCallBack");
            mLayerPackage.registerCallBack(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "SearchResultModel 销毁");
        RoutePackage.getInstance().unRegisterRouteObserver(mCallbackId);
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(mCallbackId);
        }
        if (mLayerPackage != null) {
            Logger.d(TAG, "unRegisterCallBack");
            mLayerPackage.unRegisterCallBack(getIdFromName(mViewModel.mScreenId), this);
        }
    }

    @Override
    public void onMarkClickCallBack(final PoiInfoEntity poiInfoEntity) {
        mViewModel.onMarkClickCallBack(poiInfoEntity);
    }

    /**
     * 清除扎标
     */
    public void clearLabelMark() {
        mSearchPackage.clearLabelMark();
    }

    /**
     * 是否处于算路阶段
     *
     * @return true 是 false 否
     */
    public boolean isAlongWaySearch() {
        return mSearchPackage.isAlongWaySearch();
    }

    /**
     * 获取地图类型id
     *
     * @param name 地图类型名称
     * @return 地图类型
     */
    // TODO 后期需要优化
    private MapType getIdFromName(@Nullable final String name) {
        MapType mapTypeId = MapType.MAIN_SCREEN_MAIN_MAP;
        if (TextUtils.equals(name, MapType.MAIN_SCREEN_MAIN_MAP.name())) {
            mapTypeId = MapType.MAIN_SCREEN_MAIN_MAP;
        }
        return mapTypeId;
    }

    @Override
    public void onNetSearchResult(final int taskId,String searchKey,BaseRep result) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mCallbackId: " + mCallbackId + "currentCallbackId: " +mSearchPackage.getCurrentCallbackId());
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if(AutoMapConstant.NetSearchKey.QUERY_STATION_LIST.equals(searchKey)){
                mViewModel.notifyNetSearchResult(taskId,result);
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }

    }

    @Override
    public void onNetSearchResultError(int taskId, String searchKey, String message) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mCallbackId: " + mCallbackId + "currentCallbackId: " +mSearchPackage.getCurrentCallbackId());
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if(AutoMapConstant.NetSearchKey.QUERY_STATION_LIST.equals(searchKey)){
                mViewModel.notifyNetSearchResultError(taskId,message);
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }
    }

    public void addPoiMarker(SearchResultEntity searchResultEntity){
        mSearchPackage.clearLabelMark();
        mSearchPackage.createPoiMarker(searchResultEntity.getPoiList(),0);
    }


    public PoiInfoEntity getRouteAlongInfo() {
        return mRoutePackage.getRouteAlongInfo();
    }

    public boolean isRouteAlongSearch() {
        return mRoutePackage.isRouteAlongSearch();
    }
}
