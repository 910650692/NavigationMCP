package com.sgm.navi.hmi.utils;

import android.content.Context;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.view.VelocityTracker;
import android.view.ViewConfiguration;

import com.android.utils.log.Logger;

public class AiWaysGestureManager {
    private final String TAG = "AiWaysGestureManager";
    private GestureDetector mGestureDetector;
    private ScaleGestureDetector mScaleDetector;
    private AiwaysGestureListener mListener;
    private AiwaysMultiFingerSplit mAiwaysMultiFingerSplit;
    private VelocityTracker mVelocityTracker;
    private int mMinimumFlingVelocity;
    private int mMaximumFlingVelocity;
    private int minVelocity = 50;

    public interface AiwaysGestureListener {
        default boolean onSingleTapUp(MotionEvent motionEvent) {
            return false;
        }

        default boolean onDoubleTap(MotionEvent motionEvent) {
            return false;
        }

        default void onLongPress(MotionEvent e) {

        }

        default boolean onDown(MotionEvent e) {
            return false;
        }

        default boolean onUp(MotionEvent motionEvent) {
            return false;
        }

        default boolean onScale(ScaleGestureDetector detector) {
            return false;
        }

        default boolean onScaleBegin(ScaleGestureDetector detector) {
            return false;
        }

        default void onScaleEnd(ScaleGestureDetector detector) {

        }

        default boolean singleFingeronSlipProcess(MotionEvent e1, MotionEvent e2, float dx, float dy) {
            return false;
        }

        default boolean singleFingerSlipAction(GestureEvent gestureEvent, MotionEvent startEvent, MotionEvent endEvent, float velocity) {
            return false;
        }

        default boolean mutiFingerSlipProcess(GestureEvent gestureEvent, float startX, float startY, float endX, float endY, float moveX, float moveY) {
            return false;
        }

        default boolean mutiFingerSlipAction(GestureEvent gestureEvent, float startX, float startY, float endX, float endY, float velocityX, float velocityY) {
            return false;
        }
    }

    public enum GestureEvent {
        SINGLE_GINGER_LEFT_SLIP,
        SINGLE_GINGER_RIGHT_SLIP,
        SINGLE_GINGER_UP_SLIP,
        SINGLE_GINGER_DOWN_SLIP,
        DOUBLE_GINGER,
        DOUBLE_GINGER_LEFT_SLIP,
        DOUBLE_GINGER_RIGHT_SLIP,
        DOUBLE_GINGER_UP_SLIP,
        DOUBLE_GINGER_DOWN_SLIP,
        THREE_GINGER,
        THREE_GINGER_RIGHT_SLIP,
        THREE_GINGER_LEFT_SLIP,
        THREE_GINGER_UP_SLIP,
        THREE_GINGER_DOWN_SLIP,
        SCALE_START,
        SCALE_MOVE,
        SCALE_END;
    }
    public AiWaysGestureManager(Context context, AiwaysGestureListener listener) {
        mListener = listener;
        mGestureDetector = new GestureDetector(context, new SimpleGesture(), null, true /* ignoreMultitouch */);
        mGestureDetector.setOnDoubleTapListener(new AiwaysDoubleTapListener());
        mScaleDetector = new ScaleGestureDetector(context, new ScaleListener());
        mAiwaysMultiFingerSplit = new AiwaysMultiFingerSplit();
        if (context == null) {
            mMinimumFlingVelocity = ViewConfiguration.getMinimumFlingVelocity();
            mMaximumFlingVelocity = ViewConfiguration.getMaximumFlingVelocity();
        } else {
            final ViewConfiguration configuration = ViewConfiguration.get(context);
            mMinimumFlingVelocity = configuration.getScaledMinimumFlingVelocity();
            mMaximumFlingVelocity = configuration.getScaledMaximumFlingVelocity();
        }
    }
    public boolean onTouchEvent(MotionEvent event) {
        final boolean gestureProcessed = mGestureDetector.onTouchEvent(event);
        final boolean scaleProcessed = mScaleDetector.onTouchEvent(event);
        final boolean multiFingerProcessed = mAiwaysMultiFingerSplit.onTouchEvent(event);
        if (event.getAction() == MotionEvent.ACTION_UP) {
            mListener.onUp(event);
        }
        return (gestureProcessed | scaleProcessed | multiFingerProcessed);
    }
    private class SimpleGesture extends GestureDetector.SimpleOnGestureListener {
        @Override
        public void onLongPress(MotionEvent e) {
            mListener.onLongPress(e);
        }
        @Override
        public boolean onScroll(
                MotionEvent e1, MotionEvent e2, float dx, float dy) {
            return mListener.singleFingeronSlipProcess(e1, e2, dx, dy);
        }
        @Override
        public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX,
                float velocityY) {
            if (e1 == null || e2 == null) {
                return false;
            }
            try {
                if (e1.getX() - e2.getX() > 0 && Math.abs((int)(e1.getX() - e2.getX())) > Math.abs((int)(e1.getY() - e2.getY()))
                    && Math.abs(velocityX) > minVelocity) {
                    return mListener.singleFingerSlipAction(GestureEvent.SINGLE_GINGER_LEFT_SLIP, e1, e2, Math.abs(velocityX));
                }
                if (e1.getX() - e2.getX() < 0 && Math.abs((int)(e1.getX() - e2.getX())) > Math.abs((int)(e1.getY() - e2.getY()))
                    && Math.abs(velocityX) > minVelocity) {
                    return mListener.singleFingerSlipAction(GestureEvent.SINGLE_GINGER_RIGHT_SLIP, e1, e2, Math.abs(velocityX));
                }
                if (e1.getY() - e2.getY() > 0 && Math.abs((int)(e1.getY() - e2.getY())) > Math.abs((int)(e1.getX() - e2.getX()))
                    && Math.abs(velocityY) > minVelocity) {
                    return mListener.singleFingerSlipAction(GestureEvent.SINGLE_GINGER_UP_SLIP, e1, e2, Math.abs(velocityY));
                }
                if (e1.getY() - e2.getY() < 0 && Math.abs((int)(e1.getY() - e2.getY())) > Math.abs((int)(e1.getX() - e2.getX()))
                    && Math.abs(velocityY) > minVelocity) {
                    return mListener.singleFingerSlipAction(GestureEvent.SINGLE_GINGER_DOWN_SLIP, e1, e2, Math.abs(velocityY));
                }
            } catch (Exception ex) {
                Logger.e(TAG, "onFling error", ex);
                return false;
            }
            return false;
        }
        @Override
        public boolean onDown(MotionEvent e) {
            mListener.onDown(e);
            return super.onDown(e);
        }
    }
    private class AiwaysDoubleTapListener implements GestureDetector.OnDoubleTapListener {
        @Override
        public boolean onSingleTapConfirmed(MotionEvent e) {
            return mListener.onSingleTapUp(e);
        }
        @Override
        public boolean onDoubleTap(MotionEvent e) {
            return mListener.onDoubleTap(e);
        }
        @Override
        public boolean onDoubleTapEvent(MotionEvent e) {
            return true;
        }
    }

    boolean notifyMutiFingerSlipProcess(MotionEvent startEvent, MotionEvent endEvent, float dX, float dY) {
        if (startEvent == null || endEvent == null){
            return false;
        }
        int pCount = startEvent.getPointerCount();
        int sumX = 0, sumY = 0;
        for (int i = 0; i < pCount; i++) {
                sumX += startEvent.getX(i);
                sumY += startEvent.getY(i);
        }

        int startX = sumX / pCount;
        int startY = sumY / pCount;

        sumX = sumY =0;
        pCount = endEvent.getPointerCount();
        for (int i = 0; i < pCount; i++) {
                sumX += endEvent.getX(i);
                sumY += endEvent.getY(i);
        }

        int endX = sumX / pCount;
        int endY = sumY / pCount;
        if (pCount == 2) {
            return mListener.mutiFingerSlipProcess(GestureEvent.DOUBLE_GINGER, startX, startY, endX, endY, dX, dY);
        }

        if (pCount == 3) {
            return mListener.mutiFingerSlipProcess(GestureEvent.THREE_GINGER, startX, startY, endX, endY, dX, dY);
        }
        return false;
    }

    boolean notifyMutiFingerSlipAction(MotionEvent startEvent, MotionEvent endEvent, float velocityX, float velocityY) {
        if (startEvent == null || endEvent == null) {
            Logger.e(TAG, "notifyMutiFingerSlipAction: event is null");
            return false;
        }
        int pCount = startEvent.getPointerCount();
        int sumX = 0, sumY = 0;
        for (int i = 0; i < pCount; i++) {
            sumX += startEvent.getX(i);
            sumY += startEvent.getY(i);
        }
        int startX = sumX / pCount;
        int startY = sumY / pCount;
        sumX = sumY = 0;
        pCount = endEvent.getPointerCount();
        for (int i = 0; i < pCount; i++) {
            sumX += endEvent.getX(i);
            sumY += endEvent.getY(i);
        }
        int endX = sumX / pCount;
        int endY = sumY / pCount;
        Logger.d(TAG, "startX:" + startX + " startY:"+startY + " endX: " + endX + " endY: " + endY + " velocityX:" + velocityX + " velocityY: " + velocityY,"pCount"+pCount);
        Logger.d(TAG,"pCount"+pCount);
        if (pCount == 2) {
            if (startX - endX > 0 && Math.abs(velocityX) > minVelocity
                 && Math.abs(startX - endX) > Math.abs(startY - endY)) {
                Logger.d(TAG, "DOUBLE_GINGER_LEFT_SLIP");//双指左滑

                return mListener.mutiFingerSlipAction(GestureEvent.DOUBLE_GINGER_LEFT_SLIP, startX , startY, endX, endY, velocityX, velocityY);
            }
            if (startX - endX < 0 && Math.abs(velocityX) > minVelocity
                 && Math.abs(startX - endX) > Math.abs(startY - endY)){
                Logger.d(TAG, "DOUBLE_GINGER_RIGHT_SLIP");//双指右滑
                return mListener.mutiFingerSlipAction(GestureEvent.DOUBLE_GINGER_RIGHT_SLIP, startX , startY, endX, endY, velocityX, velocityY);
            }
        }
        if (pCount == 3) {
            if (startX - endX > 0 && Math.abs(velocityX) > minVelocity
                    && Math.abs(startX - endX) > Math.abs(startY - endY)) {
                Logger.d(TAG, "THREE_GINGER_LEFT_SLIP");//三指左滑
                return mListener.mutiFingerSlipAction(GestureEvent.THREE_GINGER_LEFT_SLIP, startX , startY, endX, endY, velocityX, velocityY);
            }

            if (startX - endX < 0 && Math.abs(velocityX) > minVelocity
                    && Math.abs(startX - endX) > Math.abs(startY - endY)) {
                Logger.d(TAG, "THREE_GINGER_RIGHT_SLIP");//三指右滑
                return mListener.mutiFingerSlipAction(GestureEvent.THREE_GINGER_RIGHT_SLIP, startX , startY, endX, endY, velocityX, velocityY);
            }
        }
        return false;
    }


    private boolean isScale = false;
    private class ScaleListener extends ScaleGestureDetector.SimpleOnScaleGestureListener {
        @Override
        public boolean onScaleBegin(ScaleGestureDetector detector) {
            Logger.d(TAG, "onScaleBegin");
            return mListener.onScaleBegin(detector);
        }

        @Override
        public boolean onScale(ScaleGestureDetector detector) {
            Logger.d(TAG, "onScaling");
            float scaleFactor = detector.getScaleFactor(); // 获取缩放比例
            Logger.d(TAG, "onScaling: scaleFactor=" + scaleFactor);
            if (scaleFactor > 1.0f) {
                Logger.d(TAG, "正在放大");
                isScale = true;
            } else if (scaleFactor < 1.0f) {
                Logger.d(TAG, "正在缩小");
                isScale = true;
            }
            return mListener.onScale(detector);
        }

        @Override
        public void onScaleEnd(ScaleGestureDetector detector) {
            Logger.d(TAG, "onScaleEnd");
            mListener.onScaleEnd(detector);
        }
    }

    private class AiwaysMultiFingerSplit {
        private MotionEvent mStartMutiEvent;
        private MotionEvent mLastEvent;
        private float mLastFocusX;
        private float mLastFocusY;
        private float mDownFocusX;
        private float mDownFocusY;

        public boolean onTouchEvent(MotionEvent ev) {
            if (ev == null){
                return false;
            }
            if (mVelocityTracker == null) {
                mVelocityTracker = VelocityTracker.obtain();
            }
            mVelocityTracker.addMovement(ev);

            boolean handled = false;
            float sumX = 0, sumY = 0;
            float focusX = 0, focusY = 0;
            int pCount = ev.getPointerCount();
            Logger.d(TAG,"AiwaysMultiFingerSplit onTouchEvent pCount->"+pCount);
            if (pCount == 2 || pCount == 3) {
                for (int i = 0; i < pCount; i++) {
                        sumX += ev.getX(i);
                        sumY += ev.getY(i);
                }
                focusX = sumX / pCount;
                focusY = sumY / pCount;
            }

            switch(ev.getAction() & MotionEvent.ACTION_MASK) {
                case MotionEvent.ACTION_DOWN:
                    mDownFocusX = mLastFocusX = focusX;
                    mDownFocusY = mLastFocusY = focusY;
                    Logger.d(TAG,"AiwaysMultiFingerSplit onTouchEvent MotionEvent.ACTION_DOWN");
                    break;
                case MotionEvent.ACTION_UP:
                    if (mVelocityTracker != null) {
                        mVelocityTracker.recycle();
                        mVelocityTracker = null;
                    }
                    if (mLastEvent != null) {
                        mLastEvent = null;
                    }
                    Logger.d(TAG,"AiwaysMultiFingerSplit onTouchEvent MotionEvent.ACTION_UP");
                    break;
                case MotionEvent.ACTION_POINTER_DOWN:
                    if (pCount == 2 || pCount == 3) {
                        mDownFocusX = mLastFocusX = focusX;
                        mDownFocusY = mLastFocusY = focusY;
                        mStartMutiEvent = MotionEvent.obtain(ev);
                        mLastEvent = MotionEvent.obtain(ev);
                        Logger.d(TAG,"AiwaysMultiFingerSplit onTouchEvent ACTION_POINTER_DOWN pCount->"+pCount);
                    }
                    handled = true;
                    break;
                case MotionEvent.ACTION_POINTER_UP:
                    Logger.d(TAG, "AiwaysMultiFingerSplit onTouchEvent ACTION_POINTER_UP_"+ev.getActionIndex() + " ,onTouchEvent pCount:" + ev.getPointerCount());
                    if ((pCount-1) == 2 || (pCount-1) == 3) {
                        mDownFocusX = mLastFocusX = focusX;
                        mDownFocusY = mLastFocusY = focusY;
                        mStartMutiEvent = MotionEvent.obtain(ev);
                    }

                    Logger.d(TAG,"MotionEvent.ACTION_POINTER_UP");
                    if (pCount == 2 || pCount == 3) {
                        final VelocityTracker velocityTracker = mVelocityTracker;
                        velocityTracker.computeCurrentVelocity(1000, mMaximumFlingVelocity);
                        final int id1 = ev.getPointerId(ev.getActionIndex());
                        float sumVX = velocityTracker.getXVelocity(id1);
                        float sumVY = velocityTracker.getYVelocity(id1);
                        for (int i = 0; i < pCount; i++) {
                            if (i == ev.getActionIndex()) continue;

                            final int id2 = ev.getPointerId(i);
                            sumVX += velocityTracker.getXVelocity(id2);
                            sumVY += velocityTracker.getYVelocity(id2);
                        }
                        final float velocityX = sumVX / pCount;
                        final float velocityY = sumVY / pCount;
                        velocityTracker.clear();

                        handled |= notifyMutiFingerSlipAction(mLastEvent, ev, velocityX , velocityY);
                    }
                    break;
                case MotionEvent.ACTION_MOVE:
                    Logger.d(TAG,"MotionEvent.ACTION_MOVE"+pCount);
                    if (pCount == 2 || pCount == 3) {
                        final float scrollX = mLastFocusX - focusX;
                        final float scrollY = mLastFocusY - focusY;
                        if ((Math.abs(scrollX) >= 1) || (Math.abs(scrollY) >= 1)) {
                            handled |= notifyMutiFingerSlipProcess(mStartMutiEvent, ev, scrollX, scrollY);
                        }
                        mLastFocusX = focusX;
                        mLastFocusY = focusY;
                        handled = true;
                    }

                    break;
                default:
                    break;
            }
            return handled;
        }
    }
}
