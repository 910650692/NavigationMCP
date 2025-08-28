package com.sgm.navi.hmi.search.around;

import android.app.Application;

import androidx.annotation.NonNull;

import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseAroundSearchViewModel extends BaseViewModel<AroundSearchFragment, AroundSearchModel> {
    public BaseAroundSearchViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected AroundSearchModel initModel() {
        return new AroundSearchModel();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mView = null;
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
     * @return 车辆动力类型
     */
    public int powerType() {
        return mModel.powerType();
    }
}
