package com.sgm.navi.hmi.search.mainsearch;

import android.app.Application;

import androidx.annotation.NonNull;

import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseMainSearchViewModel extends BaseViewModel<MainSearchFragment, MainSearchModel> {
    public BaseMainSearchViewModel(@NonNull final Application application) {
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

    private final Action mRootClick = () -> {
    };

    public Action getRootClick() {
        return mRootClick;
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        return mModel.powerType();
    }
}
