package com.fy.navi.ui.base;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/22
 */
public interface IBaseModel<VM extends  IBaseViewModel> {
    void onAttachViewModel(VM baseViewModel);

    void onCreate();

    void onStart();

    void onStop();

    void onDestroy();
}
