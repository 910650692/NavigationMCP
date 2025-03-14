package com.fy.navi.hmi.search.offlinesearch;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.hmi.search.suggestion.SuggestionResultModel;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/1/25
 */
public class BaseOfflineSearchResultVm extends BaseViewModel<OfflineSearchFragment, OfflineSearchResultModel> {
    public BaseOfflineSearchResultVm(@NonNull Application application) {
        super(application);
    }

    @Override
    protected OfflineSearchResultModel initModel() {
        return new OfflineSearchResultModel();
    }

    public Action rootClick = new Action() {
        @Override
        public void call() {
        }
    };

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }
}
