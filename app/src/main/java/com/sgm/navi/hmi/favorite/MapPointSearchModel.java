package com.sgm.navi.hmi.favorite;

import android.os.Bundle;
import android.os.Parcelable;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.ui.base.BaseModel;

import java.util.UUID;

public class MapPointSearchModel extends BaseModel<MapPointSearchViewModel> implements SearchResultCallback {
    private final String mCallbackId;
    private SearchPackage mSearchPackage;

    public MapPointSearchModel() {
        this.mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
    }
    public String getCallbackId() {
        return mCallbackId;
    }


    @Override
    public void onCreate() {
        super.onCreate();
        mSearchPackage.registerCallBack(mCallbackId,this);
    }

    /**
     * createBundle
     * @param key
     * @param value
     * @return bundle
     * @param <T>
     */
    private <T extends Parcelable> Bundle createBundle(final String key, final T value) {
        final Bundle bundle = new Bundle();
        bundle.putParcelable(key, value);
        return bundle;
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
        if(getCallbackId().equals(mSearchPackage.getCurrentCallbackId())){
            final ThreadManager threadManager = ThreadManager.getInstance();
            threadManager.postUi(() -> {
                mViewModel.notifySearchResult(taskId, searchResultEntity);
            });
        }else{
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "MapPointSearch Ignoring callback for ID: " + taskId);
        }
    }

}
