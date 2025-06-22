package com.sgm.navi.hmi.search.offlinesearch;

import android.app.Application;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

/**
 * @author lww
 * @version \$Revision1.0\$
 */
public class BaseOfflineSearchResultVm extends BaseViewModel<OfflineSearchFragment, OfflineSearchResultModel> {
    public BaseOfflineSearchResultVm(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected OfflineSearchResultModel initModel() {
        return new OfflineSearchResultModel();
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
    public void notifySearchResult(final int taskId,final SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(taskId,searchResultEntity);
    }
}
