package com.fy.navi.hmi.poi;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class BasePoiDetailsViewModel extends BaseViewModel<PoiDetailsFragment, PoiDetailsModel> {
    public BasePoiDetailsViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected PoiDetailsModel initModel() {
        return new PoiDetailsModel();
    }

    public void onSearchResult(SearchResultEntity searchResultEntity) {
        mView.onSearchResult(searchResultEntity);
    }

    public Action rootClick = () -> {
    };
}
