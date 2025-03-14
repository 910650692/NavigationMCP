package com.fy.navi.ui.base;

import android.os.Bundle;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/22
 */
public interface IBaseView {
    /**
     * Activity必须实现.要在里边确认该Activity的屏幕Id.
     */
    default void onCreateBefore() {

    }

    int onLayoutId();

    int onInitVariableId();

    default int onFragmentId() {
        return 0;
    }

    void onInitView();


    default void onInitObserver() {

    }

    void onInitData();

    void addFragment(BaseFragment fragment, Bundle bundle);

    void closeFragment(boolean nextShow);

    void closeAllFragment();

    void closeAllFragmentUpRoute();

    void closeAllFragmentsUntilTargetFragment(String targetClassName);

    void closeAllFragmentAndSearchView();
}