package com.sgm.navi.ui.base;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleOwner;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

public abstract class BaseViewModel<V extends IBaseView, M extends IBaseModel> extends AndroidViewModel implements IBaseViewModel {
    protected V mView;
    protected M mModel;
    protected Application mApplication;
    public String mScreenId;

    public BaseViewModel(final @NonNull Application application) {
        super(application);
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "Constructor");
        }
        this.mApplication = application;
        mModel = initModel();
    }

    /**
     * 绑定View
     *
     * @param baseView View
     * @param screenId 页面标识
     */
    public void attachView(final V baseView, final String screenId) {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "attachView", "screenId: ", screenId);
        }
        mView = baseView;
        mModel.onAttachViewModel(this);
        mScreenId = screenId;
    }

    @Override
    public void onAny(final LifecycleOwner owner, final Lifecycle.Event event) {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onAny", event.name());
        }
    }

    @Override
    public void onCreate() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onCreate");
        }
        mModel.onCreate();
    }

    @Override
    public void onResume() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onResume");
        }
    }

    @Override
    public void onStart() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onStart");
        }
        mModel.onStart();
    }

    @Override
    public void onStop() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onStop");
        }
        mModel.onStop();
    }

    @Override
    public void onPause() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onPause");
        }
    }

    @Override
    public void onDestroy() {
        if(Logger.openLog) {
            Logger.i(getClass().getSimpleName(), "onDestroy");
        }
        mModel.onDestroy();
    }

    @Override
    public void addFragment(final BaseFragment fragment, final Bundle bundle) {
        final BaseFragment currentFragment = StackManager.getInstance().getCurrentFragment(mScreenId);
        if (!ConvertUtils.isNull(currentFragment) && ConvertUtils.equals(currentFragment.getClass().getName(), fragment.getClass().getName())) {
            if (!ConvertUtils.isNull(fragment)) {
                fragment.setArguments(bundle);
            }
        } else {
            mView.addFragment(fragment, bundle);
        }
    }

    @Override
    public void addFragment(BaseFragment fragment, Bundle bundle, boolean isHideCurFragment) {
        mView.addFragment(fragment, bundle, isHideCurFragment);
    }

    @Override
    public void addPoiDetailsFragment(BaseFragment fragment, Bundle bundle) {
        mView.addPoiDetailsFragment(fragment, bundle);
    }

    @Override
    public void closeFragment(final boolean nextShow) {
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
    public void closeAllFragmentsUntilTargetFragment(final String className) {
        mView.closeAllFragmentsUntilTargetFragment(className);
    }

    protected abstract M initModel();
}
