package com.fy.navi.mapservice.base;

public interface ServiceStateCallback {

    /**
     * Service绑定成功成功.
     */
    void onServiceConnected();

    /**
     * Service绑定失败.
     */
    void onServiceDisconnected();
}
