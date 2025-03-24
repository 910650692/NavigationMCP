package com.fy.navi.hmi.search.parking;


import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;


public class BaseTerminalParkingViewModelViewModel extends BaseViewModel<TerminalParkingFragment, TerminalParkingModel> {
    public BaseTerminalParkingViewModelViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected TerminalParkingModel initModel() {
        return new TerminalParkingModel();
    }

    private final Action mRootClick = () -> {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "rootClick: ");
    };

    public Action getRootClick() {
        return mRootClick;
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }
}
