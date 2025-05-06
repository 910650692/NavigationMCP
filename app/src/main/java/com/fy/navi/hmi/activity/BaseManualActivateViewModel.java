package com.fy.navi.hmi.activity;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.base.BaseViewModel;

public class BaseManualActivateViewModel extends BaseViewModel<ManualActivateFragment, ManualActivateModel> {
    public BaseManualActivateViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected ManualActivateModel initModel() {
        return new ManualActivateModel();
    }
}
