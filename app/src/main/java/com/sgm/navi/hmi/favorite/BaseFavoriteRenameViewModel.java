package com.sgm.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseFavoriteRenameViewModel extends BaseViewModel<FavoriteRenameFragment, FavoriteRenameModel> {
    public MutableLiveData<Boolean> mCloseViewVisibility = new MutableLiveData<>(true);
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

    @Override
    protected void onBackPressed() {
        closeFragment(true);
        mView.renameFinished();
    }
}
