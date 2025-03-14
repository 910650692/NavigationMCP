package com.fy.navi.hmi.search.searchresult;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.map.MapTypeId;
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
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class SearchResultModel extends BaseModel<SearchResultViewModel> implements SearchResultCallback, ILayerPackageCallBack {
    private static final String TAG = "SearchResultModel";
    private final SearchPackage mSearchPackage;
    private final LayerPackage mLayerPackage;
    private final String callbackId;

    public SearchResultModel() {
        callbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSearchPackage.registerCallBack(callbackId, this);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (callbackId.equals(mSearchPackage.getCurrentCallbackId())) {


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
    public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (callbackId.equals(mSearchPackage.getCurrentCallbackId())) {
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
    public void onStart() {
        super.onStart();
        if (mLayerPackage != null) {
            Logger.d(TAG, "registerCallBack");
            mLayerPackage.registerCallBack(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this, LayerType.SEARCH_LAYER);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(callbackId);
        }
        if (mLayerPackage != null) {
            Logger.d(TAG, "unRegisterCallBack");
            mLayerPackage.unRegisterCallBack(getIdFromName(mViewModel.mScreenId), this, LayerType.SEARCH_LAYER);
        }
    }

    @Override
    public void onMarkClickCallBack(PoiInfoEntity poiInfoEntity) {
        SearchResultCallback.super.onMarkClickCallBack(poiInfoEntity);
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            Logger.d(SEARCH_HMI_TAG, "onMarkClickCallBack: poiInfoEntity is null");
            return;
        }

        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfoEntity);
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
        addFragment(new PoiDetailsFragment(), bundle);
    }

    public void clearLabelMark() {
        mSearchPackage.clearLabelMark();
    }

    public boolean isAlongWaySearch() {
        return mSearchPackage.isAlongWaySearch();
    }

    // TODO 后期需要优化
    private MapTypeId getIdFromName(@Nullable String name) {
        MapTypeId mapTypeId = MapTypeId.MAIN_SCREEN_MAIN_MAP;
        if (TextUtils.equals(name, MapTypeId.MAIN_SCREEN_MAIN_MAP.name())) {
            mapTypeId = MapTypeId.MAIN_SCREEN_MAIN_MAP;
        }
        return mapTypeId;
    }

    @Override
    public void onBeforeNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        // TODO
    }

    @Override
    public void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        // TODO 图层点击事件
        Logger.d(TAG, "onNotifyClick");
    }

    @Override
    public void onAfterNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        // TODO
    }
}
