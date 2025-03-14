package com.fy.navi.hmi.search.parking;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class TerminalParkingModel extends BaseModel<TerminalParkingViewModel> implements SearchResultCallback {
    private static final String TAG = "TerminalParkingModel";
    private final SearchPackage mSearchPackage;
    private final String callbackId;

    public TerminalParkingModel() {
        callbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mSearchPackage.registerCallBack(callbackId, this);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (callbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH) {
                return;
            }
            mViewModel.notifySearchResult(searchResultEntity);
        }
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(callbackId);
        }
    }

    @Override
    public void onMarkClickCallBack(PoiInfoEntity poiInfoEntity) {
        Logger.d(SEARCH_HMI_TAG, "onMarkClickCallBack: -----");
    }
}
