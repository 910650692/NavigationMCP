package com.fy.navi.ui.view;

import android.content.Context;
import android.graphics.PointF;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.VelocityTracker;
import android.view.View;
import android.view.ViewConfiguration;
import android.view.ViewGroup;
import android.widget.Scroller;

public class SwipeMenuLayout extends ViewGroup {

    private int mTouchSlop;
    private int mHeight;
    private int mMenuViewWidth;
    private Scroller mScroller;
    private VelocityTracker mVelocityTracker;
    private final PointF mFirstP = new PointF();
    private static SwipeMenuLayout mViewCache;
    private boolean isExpandFromOutSide = false;
    private float mLastX;
    // 最小滑动速度
    private static final int SNAP_VELOCITY = 600;

    public SwipeMenuLayout(Context context) {
        this(context, null);
    }

    public SwipeMenuLayout(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SwipeMenuLayout(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context);
    }

    public static SwipeMenuLayout getViewCache() {
        return mViewCache;
    }

    private void init(Context context) {
        mTouchSlop = ViewConfiguration.get(context).getScaledTouchSlop();
        mScroller = new Scroller(context);
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        mMenuViewWidth = 0;
        mHeight = 0;
        int contentWidth = 0;
        int childCount = getChildCount();

        for (int i = 0; i < childCount; i++) {
            View childView = getChildAt(i);
            if (childView.getVisibility() != GONE) {
                if (i == 0) {
                    measureChildWithMargins(childView, widthMeasureSpec, 0, heightMeasureSpec, 0);
                    contentWidth = childView.getMeasuredWidth();
                    mHeight = Math.max(mHeight, childView.getMeasuredHeight());
                } else {
                    LayoutParams layoutParams = childView.getLayoutParams();
                    int widthSpec = MeasureSpec.makeMeasureSpec(layoutParams.width, MeasureSpec.EXACTLY);
                    int heightSpec = MeasureSpec.makeMeasureSpec(mHeight, MeasureSpec.EXACTLY);
                    childView.measure(widthSpec, heightSpec);
                    mMenuViewWidth += childView.getMeasuredWidth();
                }
            }
        }

        setMeasuredDimension(getPaddingLeft() + getPaddingRight() + contentWidth, mHeight + getPaddingTop() + getPaddingBottom());
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        int childCount = getChildCount();
        int left = getPaddingLeft();
        for (int i = 0; i < childCount; i++) {
            View childView = getChildAt(i);
            if (childView.getVisibility() != GONE) {
                childView.layout(left, getPaddingTop(), left + childView.getMeasuredWidth(), getPaddingTop() + childView.getMeasuredHeight());
                left = left + childView.getMeasuredWidth();
            }
        }
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {
        obtainVelocity(ev);
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                if (!mScroller.isFinished()) {
                    mScroller.abortAnimation();
                }
                mLastX = ev.getX();
                mFirstP.set(ev.getX(), ev.getY());
                if (mViewCache != null) {
                    if (mViewCache != this) {
                        mViewCache.smoothClose();
                        return true;
                    }
                    getParent().requestDisallowInterceptTouchEvent(true);
                }
                break;
            case MotionEvent.ACTION_MOVE:
                getParent().requestDisallowInterceptTouchEvent(true);
                mVelocityTracker.computeCurrentVelocity(1000);
                float xVelocity = mVelocityTracker.getXVelocity();
                float yVelocity = mVelocityTracker.getYVelocity();
                float x = ev.getX();
                float y = ev.getY();
                if (Math.abs(xVelocity) > SNAP_VELOCITY && Math.abs(xVelocity) > Math.abs(yVelocity) || Math.abs(x - mFirstP.x) >= mTouchSlop && Math.abs(x - mFirstP.x) > Math.abs(y - mFirstP.y)) {
                    return true;
                } else {
                    getParent().requestDisallowInterceptTouchEvent(false);
                }
                releaseVelocity();
                break;
            case MotionEvent.ACTION_CANCEL:
            case MotionEvent.ACTION_UP:
                if (this == mViewCache && !isExpandFromOutSide) {
                    if (ev.getX() < getWidth() - getScrollX()) {
                        smoothClose();
                        return true;
                    }
                }
                isExpandFromOutSide = false;
                break;
        }
        return super.onInterceptTouchEvent(ev);
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        float x = ev.getX();
        obtainVelocity(ev);
        switch (ev.getAction()) {
            case MotionEvent.ACTION_MOVE:
                float dx = mLastX - x;
                if (getScrollX() + dx > 0 && getScrollX() + dx < mMenuViewWidth) {
                    scrollBy((int) dx, 0);
                }
                mLastX = x;
                break;
            case MotionEvent.ACTION_CANCEL:
            case MotionEvent.ACTION_UP:
                mVelocityTracker.computeCurrentVelocity(1000);
                int scrollX = getScrollX();
                if (mVelocityTracker.getXVelocity() < -SNAP_VELOCITY) {
                    int delt = Math.abs(mMenuViewWidth - scrollX);
                    int t = (int) (delt / mVelocityTracker.getXVelocity() * 1000);
                    smoothExpand(t);
                } else if (mVelocityTracker.getXVelocity() >= SNAP_VELOCITY) {
                    smoothClose();
                } else if (scrollX >= mMenuViewWidth / 2) {
                    smoothExpand(500);
                } else {
                    smoothClose();
                }
                releaseVelocity();
                break;
        }
        return super.onTouchEvent(ev);
    }


    @Override
    public void computeScroll() {
        if (mScroller.computeScrollOffset()) {
            scrollTo(mScroller.getCurrX(), mScroller.getCurrY());
            invalidate();
        }
    }

    /**
     * @param event 向VelocityTracker添加MotionEvent
     * @see VelocityTracker#obtain()
     * @see VelocityTracker#addMovement(MotionEvent)
     */
    private void obtainVelocity(MotionEvent event) {
        if (null == mVelocityTracker) {
            mVelocityTracker = VelocityTracker.obtain();
        }
        mVelocityTracker.addMovement(event);
    }

    /**
     * * 释放VelocityTracker
     *
     * @see VelocityTracker#clear()
     * @see VelocityTracker#recycle()
     */
    private void releaseVelocity() {
        if (null != mVelocityTracker) {
            mVelocityTracker.clear();
            mVelocityTracker.recycle();
            mVelocityTracker = null;
        }
    }

    @Override
    protected void onDetachedFromWindow() {
        if (this == mViewCache) {
            mViewCache.smoothClose();
            mViewCache = null;
        } else {
            smoothClose();
        }
        super.onDetachedFromWindow();
    }

    @Override
    public LayoutParams generateLayoutParams(AttributeSet attrs) {
        return new MarginLayoutParams(getContext(), attrs);
    }

    /**
     * 快速关闭。
     * 用于 点击侧滑菜单上的选项,同时想让它快速关闭(删除 置顶)。
     * 这个方法在ListView里是必须调用的，
     * 在RecyclerView里，视情况而定，如果是mAdapter.notifyItemRemoved(pos)方法不用调用。
     */
    public void quickClose() {
        if (this == mViewCache) {
            mViewCache.scrollTo(0, 0);
            mViewCache = null;
        }
    }


    public void smoothExpand(int time) {
        mViewCache = SwipeMenuLayout.this;
        mScroller.startScroll(getScrollX(), 0, mMenuViewWidth - getScrollX(), 0, time);
        invalidate();
    }

    public void smoothExpandFromOutSide(int time) {
        isExpandFromOutSide = true;
        mViewCache = SwipeMenuLayout.this;
        mScroller.startScroll(getScrollX(), 0, mMenuViewWidth - getScrollX(), 0, time);
        invalidate();
    }


    /**
     * 平滑关闭
     */
    public void smoothClose() {
        mViewCache = null;
        mScroller.startScroll(getScrollX(), 0, -getScrollX(), 0, 500);
        invalidate();
    }

}
