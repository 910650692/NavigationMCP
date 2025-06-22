package com.sgm.navi.mapservice.callback;

public interface OnNaviBroadcastStateListener {

    /**
     * 引导播报状态改变回调.
     *
     * @param open true-播报打开  false-播报关闭.
     */
    void onNaviBroadcastStateChanged(boolean open);
}
