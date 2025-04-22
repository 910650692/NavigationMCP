package com.fy.navi.ui.view;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.Scroller;

public class SwipeDeleteLayout extends LinearLayout {
    private static final String TAG = "SwipeDeleteLayout";
    private View mContentView;
    private int mContentViewWidth;
    private View mDeleteView;
    private int mDeleteViewWidth;
    private View mDeleteChildView;
    private int mDeleteChildViewWidth;
    private Scroller mScroller;
    private float mStartX;
    private float mStartY;
    private OnSwipeActionListener mOnActionListener;
    private boolean mIsOpen = true;

    public SwipeDeleteLayout(Context context) {
        this(context, null);
    }

    public SwipeDeleteLayout(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SwipeDeleteLayout(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mScroller = new Scroller(context);
        setOrientation(HORIZONTAL);
    }

    private void initListener() {
        if (mContentView != null) {
            mContentView.setOnClickListener(v -> {
                close();
            });
        }
    }

    @Override
    protected void onFinishInflate() {
        super.onFinishInflate();
        if (getChildCount() != 2) {
            throw new IllegalArgumentException("SwipeDeleteLayout must have two child views.");
        }
        mContentView = getChildAt(0);
        mDeleteView = getChildAt(1);
        if (mDeleteView instanceof ViewGroup && ((ViewGroup) mDeleteView).getChildCount() > 0) {
            mDeleteChildView = ((ViewGroup) mDeleteView).getChildAt(0);
        }
        mDeleteView.setOnClickListener(v -> {
            if (mOnActionListener != null) {
                mOnActionListener.onDelete(SwipeDeleteLayout.this);
            }
        });
        initListener();
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        mContentViewWidth = mContentView.getMeasuredWidth();
        mDeleteViewWidth = mDeleteView.getMeasuredWidth();
        if (mDeleteChildView != null) {
            mDeleteChildViewWidth = mDeleteChildView.getMeasuredWidth();
        }
        ViewGroup.LayoutParams contentParams = mContentView.getLayoutParams();
        contentParams.width = MeasureSpec.getSize(widthMeasureSpec);
        mContentView.setLayoutParams(contentParams);
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {
        boolean intercept = false;
        float x = ev.getX();
        float y = ev.getY();
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mStartX = x;
                mStartY = y;
                break;
            case MotionEvent.ACTION_MOVE:
                float dx = x - mStartX;
                float dy = y - mStartY;
                if (Math.abs(dx) > Math.abs(dy) && mIsOpen) {
                    intercept = true;
                }
                break;
            default:
                break;
        }
        return intercept;
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        notifyOnDrag(ev);
        float x = ev.getX();
        float y = ev.getY();
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mStartX = x;
                mStartY = y;
                break;
            case MotionEvent.ACTION_MOVE:
                float dx = x - mStartX;
                float newScrollX = getScrollX() - dx;
                if (newScrollX < 0) {
                    newScrollX = 0;
                } else if (newScrollX > mDeleteViewWidth) {
                    newScrollX = mDeleteViewWidth;
                }
                scrollTo((int) newScrollX, 0);
                changeDelViewChildLayout();
                mStartX = x;
                break;
            case MotionEvent.ACTION_UP:
                int scrollX = getScrollX();
                int marginLeft = ((MarginLayoutParams) (mDeleteChildView.getLayoutParams())).leftMargin;
                if (scrollX > mDeleteChildViewWidth + marginLeft && scrollX < 0.5 * mContentViewWidth) {
                    // 删除按钮区域 < 滑动距离 < 50% 卡片宽度，松手后删除按钮区域固定
                    mScroller.startScroll(0, 0, scrollX, 0, 0);
                } else if (scrollX >= 0.5 * mContentViewWidth) {
                    // 50% 卡片宽度 < 滑动距离，直接删除卡片
                    if (mOnActionListener != null) {
                        mOnActionListener.onDelete(SwipeDeleteLayout.this);
                    }
                } else {
                    // 松手后卡片复原
                    mScroller.startScroll(scrollX, 0, -scrollX, 0);
                }
                invalidate();
                break;
            default:
                break;
        }
        return true;
    }

    private void notifyOnDrag(MotionEvent ev) {
        if (mOnActionListener != null) {
            final boolean isOnDrag = ev.getAction() != MotionEvent.ACTION_UP;
            mOnActionListener.onStateChanged(isOnDrag);
        }
    }

    private void changeDelViewChildLayout() {
        if (mDeleteChildView != null && mDeleteChildView.getLayoutParams() instanceof MarginLayoutParams) {
            final int dis = (int) ((mDeleteViewWidth - mDeleteChildViewWidth) / (2f * mDeleteViewWidth) * getScrollX());
            final int maxDis = (int) ((mDeleteViewWidth - mDeleteChildViewWidth) / 2f);
            MarginLayoutParams layoutParams = (MarginLayoutParams) mDeleteChildView.getLayoutParams();
            if (layoutParams != null) {
                layoutParams.setMarginStart(dis > maxDis ? maxDis : dis);
            }
            mDeleteChildView.setLayoutParams(layoutParams);
        } else {
            Log.d(TAG, "请设置合理的布局！");
        }
    }

    @Override
    public void computeScroll() {
        if (mScroller.computeScrollOffset()) {
            scrollTo(mScroller.getCurrX(), mScroller.getCurrY());
            changeDelViewChildLayout();
            invalidate();
        }
    }

    public void close() {
        if (getScrollX() != 0) {
            mScroller.startScroll(getScrollX(), 0, -getScrollX(), 0);
            invalidate();
        }
    }

    public void setOnSwipeActionListener(OnSwipeActionListener listener) {
        this.mOnActionListener = listener;
    }

    public void removeSwipeActionListener() {
        this.mOnActionListener = null;
    }

    public interface OnSwipeActionListener {
        void onDelete(SwipeDeleteLayout layout);

        /***
         * 用户正在拖拽View
         * @param onDragging
         */
        void onStateChanged(boolean onDragging);
    }

    public void closeSwipe() {
        this.mIsOpen = false;
    }

    public void openSwipe() {
        this.mIsOpen = true;
    }
}
