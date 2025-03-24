package com.fy.navi.scene.impl.navi.common;

import android.graphics.Rect;

import androidx.annotation.NonNull;


public class AutoUIViewRect {
    private Rect mOnScreenRect = new Rect();
    private Rect mOnAppRect = new Rect();
    private Rect mInParentRect = new Rect();

    public AutoUIViewRect(final Rect onScreenRect, final Rect onAppRect, final Rect inParentRect) {
        this.mOnScreenRect = onScreenRect;
        this.mOnAppRect = onAppRect;
        this.mInParentRect = inParentRect;
    }

    /**
     * 获取基于屏幕的控件矩形区域位置
     * @return rect
     */
    public Rect getLocationOnScreen() {
        return mOnScreenRect;
    }

    /**
     * 获取基于应用显示区域的控件矩形区域位置
     * @return rect
     */
    public Rect getLocationOnApplication() {
        return mOnAppRect;
    }

    /**
     * 获取基于父控件的矩形区域位置
     * @return rect
     */
    public Rect getLocationInParent() {
        return mInParentRect;
    }

    /**
     * 是否与指定的矩形区域相等
     * @param rect rect
     * @return boolean
     */
    public boolean equals(final AutoUIViewRect rect) {
        return mOnScreenRect.equals(rect.mOnScreenRect)
                && mOnAppRect.equals(rect.mOnAppRect)
                && mInParentRect.equals(rect.mInParentRect);
    }

    @NonNull
    @Override
    public String toString() {
        return "AutoUIViewRect{" +
                "mOnScreenRect=" + mOnScreenRect.toShortString() +
                ", mOnAppRect=" + mOnAppRect.toShortString() +
                ", mInParentRect=" + mInParentRect.toShortString() +
                '}';
    }
}
