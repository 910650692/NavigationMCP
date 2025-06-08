package com.fy.navi.hmi.search.offlinesearch;


import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
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
public class OfflineSearchResultModel extends BaseModel<OfflineSearchViewModel> implements SearchResultCallback {
    private final SearchPackage mSearchPackage;
    private final String mCallbackId;

    public OfflineSearchResultModel() {
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mSearchPackage.registerCallBack(mCallbackId, this);
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
//        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
////            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
//            final ThreadManager threadManager = ThreadManager.getInstance();
//            threadManager.postUi(() -> {
//                mViewModel.notifySearchResult(searchResultEntity);
//            });
////            }
//        } else {
//            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
//        }
    }

    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            final ThreadManager threadManager = ThreadManager.getInstance();
            threadManager.postUi(() -> {
                mViewModel.notifySearchResult(taskId,searchResultEntity);
            });
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId + " ,111: " + mSearchPackage.getCurrentCallbackId());
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(mCallbackId);
        }
    }
}
