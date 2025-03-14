package com.fy.navi.ui.base;

import android.os.Bundle;

import com.android.utils.log.Logger;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/22
 */
public abstract class BaseModel<VM extends IBaseViewModel> implements IBaseModel<VM> {
    protected VM mViewModel;

    public BaseModel() {
        Logger.i(getClass().getSimpleName(), "Constructor");
    }

    @Override
    public void onAttachViewModel(VM baseViewModel) {
        Logger.i(getClass().getSimpleName(), "onAttachViewModel");
        mViewModel = baseViewModel;
    }

    @Override
    public void onCreate() {
        Logger.i(getClass().getSimpleName(), "onCreate");
    }

    @Override
    public void onStart() {
        Logger.i(getClass().getSimpleName(), "onStart");
    }

    @Override
    public void onStop() {
        Logger.i(getClass().getSimpleName(), "onStop");
    }

    @Override
    public void onDestroy() {
        Logger.i(getClass().getSimpleName(), "onDestroy");
    }

    public void addFragment(BaseFragment fragment, Bundle bundle) {
        mViewModel.addFragment(fragment, bundle);
    }

    public void closeAllFragmentUpRoute() {
        mViewModel.closeAllFragmentUpRoute();
    }

    public void closeAllFragmentsUntilTargetFragment(String className) {
        mViewModel.closeAllFragmentsUntilTargetFragment(className);
    }

    public void closeFragment(boolean nextShow) {
        mViewModel.closeFragment(nextShow);
    }
}
