package com.fy.navi.hmi.search.suggestion;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/1/25
 */
public class BaseSuggestionResultVm extends BaseViewModel<SuggestionFragment, SuggestionResultModel> {
    public BaseSuggestionResultVm(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SuggestionResultModel initModel() {
        return new SuggestionResultModel();
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
