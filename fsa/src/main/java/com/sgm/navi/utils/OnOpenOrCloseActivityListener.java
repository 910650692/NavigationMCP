package com.sgm.navi.utils;

/**
 * 开启或关闭RearScreen等
 */
public interface OnOpenOrCloseActivityListener {
    void onOpenOrClose(int mapType, boolean isOpen);
}
