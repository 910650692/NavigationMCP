package com.fy.navi.hmi.search.parking;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.search.searchresult.SearchResultFragment;
import com.fy.navi.hmi.search.searchresult.SearchResultModel;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;


public class BaseTerminalParkingViewModelViewModel extends BaseViewModel<TerminalParkingFragment, TerminalParkingModel> {
    public BaseTerminalParkingViewModelViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected TerminalParkingModel initModel() {
        return new TerminalParkingModel();
    }

    public Action rootClick = () -> {
        Logger.d(SEARCH_HMI_TAG, "rootClick: ");
    };

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }
}
