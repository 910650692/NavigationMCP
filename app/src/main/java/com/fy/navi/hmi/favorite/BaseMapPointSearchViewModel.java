package com.fy.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseMapPointSearchViewModel extends BaseViewModel<MapPointSearchFragment, MapPointSearchModel>  {
    public BaseMapPointSearchViewModel(final @NonNull Application application) {
        super(application);
    }

    @Override
    protected MapPointSearchModel initModel() {
        return new MapPointSearchModel();
    }

    public Action mRootClick = () -> {
    };

   /* *//**
     * 跳转fragment
     *//*
    @Override
    public void skipFragment(int searchType, String keyword, PoiInfoEntity entity) {
    }*/

    /**
     * notifySearchResult
     * @param taskId 任务id
     * @param searchResultEntity 数据回调实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(taskId, searchResultEntity);
    }


}
