package com.sgm.navi.hmi.carconnect.help;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseCarConnectHelpViewModel extends BaseViewModel<CarConnectHelpFragment, CarConnectHelpModel> {

    public MutableLiveData<Integer> mSelectPosition = new MutableLiveData<>(0);

    public BaseCarConnectHelpViewModel(final @NonNull Application application) {
        super(application);
    }

    @Override
    protected CarConnectHelpModel initModel() {
        return new CarConnectHelpModel();
    }

    /**
     * setSelectPosition
     * @param position
     */
    public void setSelectPosition(final int position) {
        mSelectPosition.setValue(position);
    }

    public Action mClickBack = () -> {
        closeFragment(true);
    };

    @Override
    protected void onBackPressed() {
        closeFragment(true);
    }
}
