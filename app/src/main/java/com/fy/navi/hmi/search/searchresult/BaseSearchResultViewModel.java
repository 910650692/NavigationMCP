package com.fy.navi.hmi.search.searchresult;


import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;


public class BaseSearchResultViewModel extends BaseViewModel<SearchResultFragment, SearchResultModel> {
    public BaseSearchResultViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SearchResultModel initModel() {
        return new SearchResultModel();
    }

    private final Action mRootClick = () -> {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "rootClick: ");
    };

    public Action getRootClick() {
        return mRootClick;
    }

    /**
     * 搜索结果
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }

    /**
     * 静默搜索回调
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySilentSearchResult(final SearchResultEntity searchResultEntity) {
        mView.notifySilentSearchResult(searchResultEntity);
    }

    /**
     * 语音筛选排序回调
     * @param sortValue 排序条件
     */
    public void onVoicePoiSort(final String sortValue) {
        mView.onVoicePoiSort(sortValue);
    }
}
