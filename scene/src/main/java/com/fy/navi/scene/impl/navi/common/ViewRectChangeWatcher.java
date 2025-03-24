package com.fy.navi.scene.impl.navi.common;

import android.graphics.Rect;
import android.view.View;
import android.view.ViewTreeObserver;

import com.fy.navi.scene.impl.navi.inter.RectChangeListener;

/**
 * 监听控件的宽高位置变化，其中位置变化考虑相对于屏幕、应用、父控件有变化时都通知。
 * 目前监听了View.addOnLayoutChangeListener、ViewTreeObserver.addOnGlobalLayoutListener
 * @author fy
 * @version $Revision.*$
 */
public class ViewRectChangeWatcher implements ViewTreeObserver.OnGlobalLayoutListener, View.OnLayoutChangeListener {
    private final View mView;
    private RectChangeListener mListener;
    private AutoUIViewRect mCurrentRect;

    public ViewRectChangeWatcher(final View view, final RectChangeListener rectChangeListener) {
        mView = view;
        mListener = rectChangeListener;
        mView.addOnLayoutChangeListener(this);
        mView.getViewTreeObserver().addOnGlobalLayoutListener(this);
    }

    @Override
    public void onLayoutChange(final View v, final int left, final int top, final int right,
                               final int bottom, final int oldLeft, final int oldTop,
                               final int oldRight, final int oldBottom) {
        onRectChanged();
    }

    /**
     * 销毁
     */
    public void destroy() {
        // TODO 待执行监听移除动作
        mView.getViewTreeObserver().removeOnGlobalLayoutListener(this);
        mView.removeOnLayoutChangeListener(this);
        mListener = null;
    }

    @Override
    public void onGlobalLayout() {
        onRectChanged();
    }

    /**
     * 位置大小变化
     */
    private void onRectChanged() {
        final AutoUIViewRect newRect = NaviUiUtil.getAutoUIViewRect(mView);
        /**
         * 初始化或者位置大小有发生变化时才通知
         */
        if (null == mCurrentRect) {
            mCurrentRect = new AutoUIViewRect(new Rect(), new Rect(), new Rect());
            if (mListener != null) {
                mListener.onRectChange(mView,newRect, mCurrentRect);
            }
        } else if (!newRect.equals(mCurrentRect)) {
            if (mListener != null) {
                mListener.onRectChange(mView,newRect, mCurrentRect);
            }
        }
        mCurrentRect = newRect;
    }
}
