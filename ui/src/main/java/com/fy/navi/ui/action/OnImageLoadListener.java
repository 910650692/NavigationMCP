package com.fy.navi.ui.action;

public interface OnImageLoadListener {
    /**
     * 加载完成
     *
     * @param isSuccess true：加载成功  false：加载失败
     */
    void onLoadCompleted(boolean isSuccess);

    /**
     * 开始加载
     */
    void onLoadStart();
}
