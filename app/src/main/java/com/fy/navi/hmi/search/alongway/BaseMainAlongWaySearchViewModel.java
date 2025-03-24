package com.fy.navi.hmi.search.alongway;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseMainAlongWaySearchViewModel extends BaseViewModel<MainAlongWaySearchFragment, AlongWaySearchModel> {
    public BaseMainAlongWaySearchViewModel(@NonNull final Application application) {
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
}
