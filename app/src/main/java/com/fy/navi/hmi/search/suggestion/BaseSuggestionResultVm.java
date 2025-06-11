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
     * @param taskId  任务id
     * @param searchResultEntity 搜索结果实体类
     * @param isRestore 是否是切换日夜模式导致的更新回调
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity, final boolean isRestore) {
        mView.notifySearchResult(taskId, searchResultEntity, isRestore);
    }

    /**
     * 恢复fragment状态
     */
    public void onReStoreFragment() {
        mModel.onReStoreFragment();
    }

    /**
     * 编辑框内容改变回调
     * @param content 编辑框内容
     */
    public void onEditTextChanged(final String content) {
        mModel.onEditTextChanged(content);
    }
}
