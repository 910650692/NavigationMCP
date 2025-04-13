package com.fy.navi.ui.base;

import android.os.Bundle;

public interface IBaseView {
    /**
     * Activity必须实现.要在里边确认该Activity的屏幕Id.
     */
    default void onCreateBefore() {

    }

    /**
     * Activity必须实现.
     *
     * @return 返回Activity布局文件Id.
     */
    int onLayoutId();

    /**
     * Activity必须实现.
     *
     * @return 返回Activity布局文件Id.
     */
    int onInitVariableId();

    /**
     * Fragment必须实现.
     *
     * @return 返回Fragment布局文件Id.
     */
    default int onFragmentId() {
        return 0;
    }

    /**
     * Activity必须实现.
     */
    void onInitView();

    /**
     * Activity必须实现.
     */
    default void onInitObserver() {

    }

    /**
     * onInitData
     */
    void onInitData();

    /**
     * Activity必须实现.
     *
     * @param fragment fragment
     * @param bundle   参数
     */
    void addFragment(BaseFragment fragment, Bundle bundle);

    /**
     * Activity必须实现.
     *
     * @param fragment fragment
     * @param bundle   参数
     * @param isHideCurFragment   是否隐藏当前fragment
     */
    void addFragment(BaseFragment fragment, Bundle bundle, boolean isHideCurFragment);

    /**
     * Activity必须实现.
     *
     * @param fragment fragment
     * @param bundle   参数
     */
    void addPoiDetailsFragment(BaseFragment fragment, Bundle bundle);

    /**
     * Activity必须实现.
     * <p>
     * 关闭当前Fragment
     * </>
     *
     * @param nextShow 是否显示下一个Fragment
     */
    void closeFragment(boolean nextShow);

    /**
     * @param bundle bundle
     */
    default void closeFragment(Bundle bundle) {

    }

    /**
     * Activity必须实现.
     */
    void closeAllFragment();

    /**
     * Activity必须实现.
     */
    void closeAllFragmentUpRoute();

    /**
     * Activity必须实现.
     */
    void closeAllFragmentUpNavi();

    /**
     * Activity必须实现.
     *
     * @param targetClassName 目标Fragment的ClassName
     */
    void closeAllFragmentsUntilTargetFragment(String targetClassName);

    /**
     * Activity必须实现.
     */
    void closeAllFragmentAndSearchView();

    /**
     * Activity 必须实现
     * 显示当前被hide的fragment
     */
    void showCurrentFragment();
}