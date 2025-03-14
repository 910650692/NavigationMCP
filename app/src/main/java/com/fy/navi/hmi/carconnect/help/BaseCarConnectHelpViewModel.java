package com.fy.navi.hmi.carconnect.help;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseCarConnectHelpViewModel extends BaseViewModel<CarConnectHelpFragment, CarConnectHelpModel> {

    public MutableLiveData<Integer> selectPosition = new MutableLiveData<>(0);

    public BaseCarConnectHelpViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected CarConnectHelpModel initModel() {
        return new CarConnectHelpModel();
    }

    public void setSelectPosition(int position) {
        selectPosition.setValue(position);
    }

    public Action clickBack = () -> {
        closeFragment(true);
    };
}
