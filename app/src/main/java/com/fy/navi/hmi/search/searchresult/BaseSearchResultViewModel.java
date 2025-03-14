package com.fy.navi.hmi.search.searchresult;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;


public class BaseSearchResultViewModel extends BaseViewModel<SearchResultFragment, SearchResultModel> {
    public BaseSearchResultViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SearchResultModel initModel() {
        return new SearchResultModel();
    }

    public Action rootClick = () -> {
        Logger.d(SEARCH_HMI_TAG, "rootClick: ");
    };

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }

    public void notifySilentSearchResult(SearchResultEntity searchResultEntity) {
        mView.notifySilentSearchResult(searchResultEntity);
    }
}
