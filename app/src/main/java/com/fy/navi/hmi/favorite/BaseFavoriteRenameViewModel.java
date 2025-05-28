package com.fy.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseFavoriteRenameViewModel extends BaseViewModel<FavoriteRenameFragment, FavoriteRenameModel> {
    public BaseFavoriteRenameViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected FavoriteRenameModel initModel() {
        return new FavoriteRenameModel();
    }

    public Action mCloseFragment = () -> {
        closeFragment(true);
    };

    public Action mClearName = () -> {
        mView.clearEditText();
    };

    public Action mRenameFinish = () -> {
        closeFragment(true);
        mView.renameFinished();
    };

}
