package com.fy.navi.hmi.search.suggestion;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @author lww
 * @version \$Revision1.0\$
 */
public class BaseSuggestionResultVm extends BaseViewModel<SuggestionFragment, SuggestionResultModel> {
    public BaseSuggestionResultVm(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SuggestionResultModel initModel() {
        return new SuggestionResultModel();
    }

    private final Action mRootClick = new Action() {
        @Override
        public void call() {
        }
    };

    public Action getRootClick() {
        return mRootClick;
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }
}
