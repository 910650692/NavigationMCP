package com.fy.navi.hmi.search.mainsearch;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseMainSearchViewModel extends BaseViewModel<MainSearchFragment, MainSearchModel> {
    public BaseMainSearchViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    protected MainSearchModel initModel() {
        return new MainSearchModel();
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
