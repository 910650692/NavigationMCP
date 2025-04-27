package com.fy.navi.hmi.search.suggestion;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
public class SuggestionResultModel extends BaseModel<SuggestionResultViewModel> implements SearchResultCallback {
    private final SearchPackage mSearchPackage;
    private final String mCallbackId;
    private int mTaskId;
    private SearchResultEntity mSearchResultEntity;
    private String mContent;

    public SuggestionResultModel() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "SearchResultModel 初始化" + this);
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mSearchPackage.registerCallBack(mCallbackId, this);
    }

    /**
     * 恢复fragment状态
     */
    public void onReStoreFragment() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onReStoreFragment: " + mSearchResultEntity);
        if (!ConvertUtils.isEmpty(mSearchResultEntity) && !ConvertUtils.isEmpty(mContent)) {
            final ThreadManager threadManager = ThreadManager.getInstance();
            threadManager.postUi(() -> mViewModel.notifySearchResult(mSearchResultEntity));
        }
    }

    /**
     * 编辑框内容改变回调
     * @param content 编辑框内容
     */
    public void onEditTextChanged(final String content) {
        mContent = content;
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
                mTaskId = taskId;
                mSearchResultEntity = searchResultEntity;
                final ThreadManager threadManager = ThreadManager.getInstance();
                threadManager.postUi(() -> {
                    mViewModel.notifySearchResult(searchResultEntity);
                });
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "SuggestionResultModel 销毁");
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(mCallbackId);
        }
    }
}
