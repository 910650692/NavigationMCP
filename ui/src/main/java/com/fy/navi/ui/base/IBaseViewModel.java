package com.fy.navi.ui.base;

import android.os.Bundle;

import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleObserver;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.OnLifecycleEvent;

public interface IBaseViewModel extends LifecycleObserver {

    /**
     * 生命周期回调
     *
     * @param owner LifecycleOwner
     * @param event Lifecycle
     */
    @OnLifecycleEvent(Lifecycle.Event.ON_ANY)
    void onAny(LifecycleOwner owner, Lifecycle.Event event);

    /**
     * 创建
     */
    @OnLifecycleEvent(Lifecycle.Event.ON_CREATE)
    void onCreate();

    /**
     * 创建
     */
    @OnLifecycleEvent(Lifecycle.Event.ON_RESUME)
    void onResume();

    /**
     * 创建
     */
    @OnLifecycleEvent(Lifecycle.Event.ON_START)
    void onStart();

    /**
     * 启动
     */
    @OnLifecycleEvent(Lifecycle.Event.ON_STOP)
    void onStop();

    /**
     * 暂停
     */
    @OnLifecycleEvent(Lifecycle.Event.ON_PAUSE)
    void onPause();

    /**
     * 销毁
     */
    @OnLifecycleEvent(Lifecycle.Event.ON_DESTROY)
    void onDestroy();

    /**
     * 添加Fragment
     *
     * @param fragment 添加的Fragment
     * @param bundle   携带的参数
     */
    void addFragment(BaseFragment fragment, Bundle bundle);

    /**
     * 添加Fragment
     *
     * @param fragment          添加的Fragment
     * @param bundle            携带的参数
     * @param isHideCurFragment 是否隐藏当前fragment
     */
    void addFragment(BaseFragment fragment, Bundle bundle, boolean isHideCurFragment);

    void addPoiDetailsFragment(BaseFragment fragment, Bundle bundle);

    /**
     * 关闭Fragment
     *
     * @param nextShow 下一个Fragment是否显示
     */
    void closeFragment(boolean nextShow);

    /**
     * 关闭所有Fragment
     */
    void closeAllFragment();

    /**
     * 关闭所有Fragment，并关闭SearchView
     */
    void closeAllFragmentUpRoute();

    /**
     * 关闭所有Fragment，并关闭SearchView
     *
     * @param className 目标Fragment的类名
     */
    void closeAllFragmentsUntilTargetFragment(String className);
}
