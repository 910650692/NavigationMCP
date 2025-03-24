package com.fy.navi.mapservice.apimanager;

public interface IEngineStatusCallback {

    /**
     * 对外接口初始化成功.
     */
    void onInitSuccess();

    /**
     * 对外接口初始化失败.
     */
    void onInitFailed();
}
