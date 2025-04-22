package com.fy.navi.scene;

import android.content.Context;
import android.os.Bundle;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.ViewDataBinding;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.LifecycleRegistry;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseActivity;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.ui.view.SkinConstraintLayout;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public abstract class BaseSceneView<VB extends ViewDataBinding, VM extends BaseSceneModel> extends SkinConstraintLayout implements LifecycleOwner {
    private LifecycleRegistry mLifecycleRegistry = new LifecycleRegistry(this);
    protected VB mViewBinding;
    protected VM mScreenViewModel;
    protected MapType mMapTypeId = MapType.MAIN_SCREEN_MAIN_MAP;
    protected StackManager mStackManager;

    public BaseSceneView(@NonNull Context context) {
        this(context, null);
    }

    public BaseSceneView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BaseSceneView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mStackManager = StackManager.getInstance();
        mViewBinding = createViewBinding(LayoutInflater.from(context), this);
        mScreenViewModel = initSceneImpl();
        mLifecycleRegistry.addObserver(mScreenViewModel);
    }

    @NonNull
    @Override
    public Lifecycle getLifecycle() {
        return mLifecycleRegistry;
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        Logger.d(getClass().getSimpleName(), "Scene onCreate start");
        onCreate();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        Logger.d(getClass().getSimpleName(), "Scene onDestroy start");
        onDestroy();
    }

    protected abstract VB createViewBinding(LayoutInflater inflater, ViewGroup viewGroup);

    protected abstract VM initSceneImpl();

    protected abstract void setInitVariableId();

    protected abstract void initObserver();

    public void setScreenId(final MapType mapTypeId) {
        this.mMapTypeId = mapTypeId;
        mScreenViewModel.setScreenId(mapTypeId);
    }

    protected void addFragment(BaseFragment fragment, Bundle bundle) {
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.addFragment(fragment, bundle);
    }

    protected void addFragment(BaseFragment fragment, Bundle bundle, boolean isHideCurFragment) {
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.addFragment(fragment, bundle, isHideCurFragment);
    }

    protected void addPoiDetailsFragment(BaseFragment fragment, Bundle bundle) {
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.addPoiDetailsFragment(fragment, bundle);
    }

    protected void closeAllFragmentsUntilTargetFragment(String className) {
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.closeAllFragmentsUntilTargetFragment(className);
    }

    protected void closeAllFragment() {
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.closeAllFragment();
    }

    protected void closeCurrentFragment() {
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.closeFragment(true);
    }

    protected void closeAllFragmentAndSearchView() {
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.closeAllFragmentAndSearchView();
    }

    protected void showCurrentFragment() {
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.showCurrentFragment();
    }

    public void showSelfParkingView(){
        BaseActivity activity = mStackManager.getCurrentActivity(mMapTypeId.name());
        activity.showParkingView();
    }

    public void onCreate() {
        setInitVariableId();
        initObserver();
        Logger.d(getClass().getSimpleName(), "Scene onCreate end");
        mLifecycleRegistry.handleLifecycleEvent(Lifecycle.Event.ON_CREATE);
    }

    public void onDestroy() {
        mViewBinding = null;
        mScreenViewModel = null;
        Logger.d(getClass().getSimpleName(), "Scene onDestroy end");
        mLifecycleRegistry.handleLifecycleEvent(Lifecycle.Event.ON_DESTROY);
    }
}