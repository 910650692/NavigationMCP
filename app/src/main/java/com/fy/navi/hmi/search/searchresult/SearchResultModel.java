package com.fy.navi.hmi.search.searchresult;


import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

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

    private int mTaskId;
    private SearchResultEntity mSearchResultEntity;

    public SearchResultModel() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "SearchResultModel 初始化" + this);
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
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
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onReStoreFragment: " + mSearchResultEntity);
        if (!ConvertUtils.isEmpty(mSearchResultEntity)) {
            final ThreadManager threadManager = ThreadManager.getInstance();
            threadManager.postUi(() -> mViewModel.notifySearchResult(mTaskId, mSearchResultEntity));
        }
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        Logger.d("huangli: ","CurrentCallbackId: "+mSearchPackage.getCurrentCallbackId());
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
                mViewModel.notifySilentSearchResult(searchResultEntity);
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
//        mViewModel.onMarkClickCallBack(index);
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
    public void onNetSearchResult(BaseRep result) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            mViewModel.notifyNetSearchResult(result);
        }
    }
}
