package com.fy.navi.hmi.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;
import android.os.Parcelable;

import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

public class MapPointSearchModel extends BaseModel<MapPointSearchViewModel> implements SearchResultCallback {
    private final String callbackId;
    private SearchPackage mSearchPackage;

    public MapPointSearchModel() {
        this.callbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mSearchPackage.registerCallBack(callbackId,this);
    }
    public String getCallbackId() {
        return callbackId;
    }

    private <T extends Parcelable> Bundle createBundle(String key, T value) {
        Bundle bundle = new Bundle();
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
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if(getCallbackId().equals(mSearchPackage.getCurrentCallbackId())){
            mViewModel.notifySearchResult(searchResultEntity);
        }else{
            Logger.d(SEARCH_HMI_TAG, "MapPointSearch Ignoring callback for ID: " + taskId);
        }
    }
}
