package com.sgm.navi.ui.base;

import android.os.Bundle;

import com.android.utils.log.Logger;

public abstract class BaseModel<VM extends IBaseViewModel> implements IBaseModel<VM> {
    protected VM mViewModel;

    public BaseModel() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "Constructor");
        }
    }

    @Override
    public void onAttachViewModel(final VM baseViewModel) {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onAttachViewModel");
        }
        mViewModel = baseViewModel;
    }

    @Override
    public void onCreate() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onCreate");
        }
    }

    @Override
    public void onStart() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onStart");
        }
    }

    @Override
    public void onStop() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onStop");
        }
    }

    @Override
    public void onDestroy() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onDestroy");
        }
    }

    /**
     * 添加Fragment
     *
     * @param fragment BaseFragment
     * @param bundle   Bundle
     */
    public void addFragment(final BaseFragment fragment, final Bundle bundle) {
        mViewModel.addFragment(fragment, bundle);
    }

    public void addPoiDetailsFragment(final BaseFragment fragment, final Bundle bundle) {
        mViewModel.addPoiDetailsFragment(fragment, bundle);
    }

    /**
     * 关闭所有Fragment
     */
    public void closeAllFragmentUpRoute() {
        mViewModel.closeAllFragmentUpRoute();
    }

    /**
     * 关闭所有Fragment
     *
     * @param className 目标Fragment的类名
     */
    public void closeAllFragmentsUntilTargetFragment(final String className) {
        mViewModel.closeAllFragmentsUntilTargetFragment(className);
    }

    /**
     * 关闭Fragment
     *
     * @param nextShow 下一个Fragment是否显示
     */
    public void closeFragment(final boolean nextShow) {
        mViewModel.closeFragment(nextShow);
    }
}
