package com.fy.navi.ui.base;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleOwner;

import com.android.utils.log.Logger;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/22
 */
public abstract class BaseViewModel<V extends IBaseView, M extends IBaseModel> extends AndroidViewModel implements IBaseViewModel {
    protected V mView;
    protected M mModel;
    protected Application mApplication;
    public String mScreenId;

    public BaseViewModel(@NonNull Application application) {
        super(application);
        Logger.i(getClass().getSimpleName(), "Constructor");
        this.mApplication = application;
        mModel = initModel();
    }

    public void attachView(V baseView, String screenId) {
        Logger.i(getClass().getSimpleName(), "attachView");
        mView = baseView;
        mModel.onAttachViewModel(this);
        mScreenId = screenId;
    }

    @Override
    public void onAny(LifecycleOwner owner, Lifecycle.Event event) {
        Logger.i(getClass().getSimpleName(), "onAny" + event.name());
    }

    @Override
    public void onCreate() {
        Logger.i(getClass().getSimpleName(), "onCreate");
        mModel.onCreate();
    }

    @Override
    public void onResume() {
        Logger.i(getClass().getSimpleName(), "onResume");
    }

    @Override
    public void onStart() {
        Logger.i(getClass().getSimpleName(), "onStart");
        mModel.onStart();
    }

    @Override
    public void onStop() {
        Logger.i(getClass().getSimpleName(), "onStop");
        mModel.onStop();
    }

    @Override
    public void onPause() {
        Logger.i(getClass().getSimpleName(), "onPause");
    }

    @Override
    public void onDestroy() {
        Logger.i(getClass().getSimpleName(), "onDestroy");
        mModel.onDestroy();
    }

    @Override
    public void addFragment(BaseFragment fragment, Bundle bundle){
       mView.addFragment(fragment, bundle);
    }

    @Override
    public void closeFragment(boolean nextShow){
        mView.closeFragment(nextShow);
    }

    @Override
    public void closeAllFragment() {
        mView.closeAllFragment();
    }

    @Override
    public void closeAllFragmentUpRoute() {
        mView.closeAllFragmentUpRoute();
    }

    @Override
    public void closeAllFragmentsUntilTargetFragment(String className) {
        mView.closeAllFragmentsUntilTargetFragment(className);
    }

    protected abstract M initModel();
}
