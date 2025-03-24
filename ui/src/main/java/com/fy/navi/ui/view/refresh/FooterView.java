package com.fy.navi.ui.view.refresh;

import android.view.View;

public interface FooterView {

    /**
     * 开始下拉
     */
    void begin();

    /**
     * 回调的精度,单位为px
     *
     * @param progress 当前高度
     * @param all      总高度   为默认高度的2倍
     */
    void progress(float progress, float all);

    /**
     * 结束下拉
     *
     * @param progress 当前高度
     * @param all      总高度   为默认高度的2倍
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
     * 设置加载更多提示
     *
     * @param tips 提示
     */
    void setLoadMoreTips(String tips);

    /**
     * 设置是否可以加载更多
     *
     * @param canLoadMore 是否可以加载更多
     */
    void canLoadMore(boolean canLoadMore);
}
