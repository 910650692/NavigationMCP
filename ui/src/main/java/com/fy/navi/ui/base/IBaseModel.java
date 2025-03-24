package com.fy.navi.ui.base;

public interface IBaseModel<VM extends IBaseViewModel> {
    /**
     * 绑定ViewModel
     *
     * @param baseViewModel IBaseViewModel
     */
    void onAttachViewModel(VM baseViewModel);

    /**
     * onCreate
     */
    void onCreate();

    /**
     * onStart
     */
    void onStart();

    /**
     * onStop
     */
    void onStop();

    /**
     * onDestroy
     */
    void onDestroy();
}
