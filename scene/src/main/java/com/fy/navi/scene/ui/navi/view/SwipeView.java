package com.fy.navi.scene.ui.navi.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.GestureDetector;
import android.view.MotionEvent;

import com.android.utils.log.Logger;
import com.fy.navi.ui.view.SkinFrameLayout;

public class SwipeView extends SkinFrameLayout {
    private GestureDetector mGestureDetector;
    private DownSwipeListener mDownSwipeListener;

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

    public interface DownSwipeListener {
        void onDownSwipe();
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
                    if (mDownSwipeListener != null) {
                        mDownSwipeListener.onDownSwipe();
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
        return true;
    }

    public void setDownSwipeListener(DownSwipeListener listener) {
        this.mDownSwipeListener = listener;
    }

}
