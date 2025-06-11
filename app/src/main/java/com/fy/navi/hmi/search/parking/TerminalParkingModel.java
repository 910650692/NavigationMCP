package com.fy.navi.hmi.search.parking;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
public class TerminalParkingModel extends BaseModel<TerminalParkingViewModel> implements SearchResultCallback {
    private static final String TAG = "TerminalParkingModel";
    private final SearchPackage mSearchPackage;
    private final String mCallbackId;
    private SearchResultEntity mSearchResultEntity;
    private int mTaskId;

    public TerminalParkingModel() {
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
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH) {
                return;
            }
            final ThreadManager threadManager = ThreadManager.getInstance();
            threadManager.postUi(() -> {
                mTaskId = taskId;
                mSearchResultEntity = searchResultEntity;
                mViewModel.notifySearchResult(taskId, searchResultEntity);

            });
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onSearchResult mCallbackId: "
                    + mCallbackId + " ,currentId: " + mSearchPackage.getCurrentCallbackId());
        }
    }

    /**
     * 恢复fragment状态
     */
    public void onReStoreFragment() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onReStoreFragment: " + mSearchResultEntity);
        if (!ConvertUtils.isEmpty(mSearchResultEntity)) {
            final ThreadManager threadManager = ThreadManager.getInstance();
            threadManager.postUi(() -> mViewModel.notifySearchResult(mTaskId, mSearchResultEntity));
        }
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(mCallbackId);
        }
    }

    @Override
    public void onMarkTerminalParkClickCallBack(final int index) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onMarkTerminalParkClickCallBack: " + index);
        mViewModel.onMarkTerminalParkClickCallBack(index);

    }
}
