package com.fy.navi.hmi.search.alongway;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

public class AlongWaySearchModel extends BaseModel<AlongWaySearchViewModel> implements SearchResultCallback {
    private final CalibrationPackage mCalibrationPackage;
    private final String mCallbackId;
    private final SearchPackage mSearchPackage;
    private int mTaskId;
    private SearchResultEntity mSearchResultEntity;
    public AlongWaySearchModel() {
        mCalibrationPackage = CalibrationPackage.getInstance();
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mSearchPackage.registerCallBack(mCallbackId, this);
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 车辆动力类型
     */
    public int powerType() {
        return mCalibrationPackage.powerType();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(mCallbackId);
        }
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
                mTaskId = taskId;
                mSearchResultEntity = searchResultEntity;
                final ThreadManager threadManager = ThreadManager.getInstance();
                threadManager.postUi(() -> {
                    if (mViewModel instanceof AlongWaySearchViewModel) {
                        mViewModel.notifySearchResult(searchResultEntity);
                    }
                });
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }
    }


}
