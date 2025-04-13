package com.fy.navi.hmi.favorite;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

public class HomeCompanyModel extends BaseModel<HomeCompanyViewModel> implements SearchResultCallback {
    private final String mCallbackId;
    private SearchPackage mSearchPackage;

    public HomeCompanyModel() {
        this.mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mSearchPackage.registerCallBack(mCallbackId,this);
    }
    public String getCallbackId() {
        return mCallbackId;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(getCallbackId());
        }
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "onSearchResult=> searchResultEntity: ", searchResultEntity);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "homeCompany Ignoring callback for ID: " + mSearchPackage.getCurrentCallbackId()
            + " getCallbackId: " + getCallbackId());
        if (getCallbackId().equals(mSearchPackage.getCurrentCallbackId())) {
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH) {
                final ThreadManager threadManager = ThreadManager.getInstance();
                threadManager.postUi(() -> {
                    mViewModel.notifySearchResult(searchResultEntity);
                });
            }
            //我的位置设置
//            if(searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH){
//                onNaviClick(searchResultEntity.getPoiList().get(0));
//            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "homeCompany Ignoring callback for ID: " + taskId + " getCallbackId: " + getCallbackId());
        }
    }
}
