package com.fy.navi.hmi.search.alongway;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseAlongWaySearchViewModel extends BaseViewModel<AlongWaySearchFragment, AlongWaySearchModel>  {
    public BaseAlongWaySearchViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected AlongWaySearchModel initModel() {
        return new AlongWaySearchModel();
    }

    public Action rootClick = () -> {
    };

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     */
    public int powerType() {
        return mModel.powerType();
    }
}
