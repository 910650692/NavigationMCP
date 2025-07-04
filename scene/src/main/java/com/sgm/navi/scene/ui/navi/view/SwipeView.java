package com.sgm.navi.scene.ui.navi.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.GestureDetector;
import android.view.MotionEvent;

import com.sgm.navi.ui.view.SkinFrameLayout;

public class SwipeView extends SkinFrameLayout {
    private GestureDetector mGestureDetector;
    private DownSwipeAndClickListener mDownSwipeAndClickListener;
    private float mDownX, mDownY;
    private long mDownTime;
    private static final int CLICK_THRESHOLD = 200; // ms
    private static final int MOVE_THRESHOLD = 10; // px

    public SwipeView(final android.content.Context context) {
        super(context);
    }

    public SwipeView(final android.content.Context context, final android.util.AttributeSet attrs) {
        super(context, attrs);
        init(context);
    }

    public SwipeView(final android.content.Context context, final android.util.AttributeSet attrs,
                     final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public interface DownSwipeAndClickListener {
        void onDownSwipe();

        void onClick();
    }


    private void init(Context context) {

        mGestureDetector = new GestureDetector(context, new GestureDetector.SimpleOnGestureListener() {
            private static final int SWIPE_THRESHOLD = 100;
            private static final int SWIPE_VELOCITY_THRESHOLD = 100;

            @Override
            public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
                if (e1 == null || e2 == null) return false;
                float diffY = e2.getY() - e1.getY();
                float diffX = Math.abs(e2.getX() - e1.getX());
                if (diffY > SWIPE_THRESHOLD && Math.abs(velocityY) >
                        SWIPE_VELOCITY_THRESHOLD && diffY > diffX) {
                    if (mDownSwipeAndClickListener != null) {
                        mDownSwipeAndClickListener.onDownSwipe();
                    }
                    return true;
                }
                return false;
            }
        });

    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        mGestureDetector.onTouchEvent(event);
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mDownX = event.getX();
                mDownY = event.getY();
                mDownTime = System.currentTimeMillis();
                break;
            case MotionEvent.ACTION_UP:
                float upX = event.getX();
                float upY = event.getY();
                long upTime = System.currentTimeMillis();
                if (Math.abs(upX - mDownX) < MOVE_THRESHOLD &&
                        Math.abs(upY - mDownY) < MOVE_THRESHOLD &&
                        (upTime - mDownTime) < CLICK_THRESHOLD) {
                    if (mDownSwipeAndClickListener != null) {
                        mDownSwipeAndClickListener.onClick();
                    }
                }
                break;
            default:
                break;
        }
        return true;
    }

    public void setDownSwipeListener(DownSwipeAndClickListener listener) {
        this.mDownSwipeAndClickListener = listener;
    }

}
