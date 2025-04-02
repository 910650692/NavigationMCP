package com.fy.navi.hmi.search.searchresult;


import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.refix.LayerType;
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

    public SearchResultModel() {
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSearchPackage.registerCallBack(mCallbackId, this);
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
                mViewModel.notifySearchResult(searchResultEntity);
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
            mLayerPackage.registerCallBack(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this, LayerType.LAYER_SEARCH);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(mCallbackId);
        }
        if (mLayerPackage != null) {
            Logger.d(TAG, "unRegisterCallBack");
            mLayerPackage.unRegisterCallBack(getIdFromName(mViewModel.mScreenId), this, LayerType.LAYER_SEARCH);
        }
    }

    @Override
    public void onMarkClickCallBack(final PoiInfoEntity poiInfoEntity) {
        SearchResultCallback.super.onMarkClickCallBack(poiInfoEntity);
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onMarkClickCallBack: poiInfoEntity is null");
            return;
        }

        final Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfoEntity);
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
        addPoiDetailsFragment(new PoiDetailsFragment(), bundle);
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
    public void onBeforeNotifyClick(final MapType mapTypeId, final GemBaseLayer layer, final GemLayerItem item) {
        // TODO
    }

    @Override
    public void onNotifyClick(final MapType mapTypeId, final GemBaseLayer layer, final GemLayerItem item) {
        // TODO 图层点击事件
        Logger.d(TAG, "onNotifyClick");
    }

    @Override
    public void onAfterNotifyClick(final MapType mapTypeId, final GemBaseLayer layer, final GemLayerItem item) {
        // TODO
    }
}
