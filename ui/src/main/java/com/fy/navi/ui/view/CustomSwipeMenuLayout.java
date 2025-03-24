package com.fy.navi.ui.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;

public class CustomSwipeMenuLayout extends SwipeMenuLayout {

    private boolean mSwipeEnabled = true;

    public CustomSwipeMenuLayout(final Context context) {
        super(context);
    }

    public CustomSwipeMenuLayout(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public CustomSwipeMenuLayout(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void setSwipeEnabled(final boolean enabled) {
        this.mSwipeEnabled = enabled;
    }

    @Override
    public boolean onInterceptTouchEvent(final MotionEvent ev) {
        // 根据isSwipeEnabled决定是否拦截触摸事件
        return mSwipeEnabled && super.onInterceptTouchEvent(ev);
    }

    @Override
    public boolean onTouchEvent(final MotionEvent event) {
        // 根据isSwipeEnabled决定是否处理触摸事件
        return mSwipeEnabled && super.onTouchEvent(event);
    }

}
