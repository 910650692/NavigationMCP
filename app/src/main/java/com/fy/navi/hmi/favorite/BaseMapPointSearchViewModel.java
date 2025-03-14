package com.fy.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseMapPointSearchViewModel extends BaseViewModel<MapPointSearchFragment, MapPointSearchModel>  {
    public BaseMapPointSearchViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected MapPointSearchModel initModel() {
        return new MapPointSearchModel();
    }

    public Action rootClick = () -> {
    };

   /* *//**
     * 跳转fragment
     *//*
    @Override
    public void skipFragment(int searchType, String keyword, PoiInfoEntity entity) {
    }*/

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }

}
