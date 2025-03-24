package com.fy.navi.ui.view.refresh;

import android.view.View;

public interface HeadView {

    /**
     * 开始下拉
     */
    void begin();

    /**
     * 回调的精度,单位为px
     *
     * @param progress 当前高度
     * @param all      总高度
     */
    void progress(float progress, float all);

    /**
     * 结束下拉
     *
     * @param progress 当前高度
     * @param all      总高度
     */
    void finishing(float progress, float all);

    /**
     * 下拉完毕
     */
    void loading();

    /**
     * 看不见的状态
     */
    void normal();

    /**
     * 获取View
     *
     * @return View
     */
    View getView();

    /**
     * 设置下拉提示
     *
     * @param tips 下拉提示
     */
    void setRefreshTips(String tips);

    /**
     * 设置是否正在刷新
     *
     * @param isRefresh 是否正在刷新
     */
    void setRefresh(boolean isRefresh);
}
