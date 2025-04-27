package com.fy.navi.hmi.search.alongway;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseAlongWaySearchViewModel extends BaseViewModel<AlongWaySearchFragment, AlongWaySearchModel>  {
    public BaseAlongWaySearchViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected AlongWaySearchModel initModel() {
        return new AlongWaySearchModel();
    }

    public Action getRootClick() {
        return mRootClick;
    }

    private final Action mRootClick = () -> {
    };

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 汽车动力类型
     */
    public int powerType() {
        return mModel.powerType();
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }
}
