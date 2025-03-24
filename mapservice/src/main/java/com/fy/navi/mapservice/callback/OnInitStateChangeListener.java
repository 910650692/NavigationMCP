package com.fy.navi.mapservice.callback;

public interface OnInitStateChangeListener {

    /**
     * 初始化成功，包括绑定service和navi引擎初始化.
     */
    void onInitSuccess();

    /**
     * 初始化失败.
     */
    void onFailure();
}
