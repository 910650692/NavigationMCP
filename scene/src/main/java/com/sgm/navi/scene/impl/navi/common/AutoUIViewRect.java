package com.sgm.navi.scene.impl.navi.common;

import android.graphics.Rect;

import androidx.annotation.NonNull;

import java.util.Objects;


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
     * @param o rect
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AutoUIViewRect that = (AutoUIViewRect) o;
        return Objects.equals(mOnScreenRect, that.mOnScreenRect)
                && Objects.equals(mOnAppRect, that.mOnAppRect)
                && Objects.equals(mInParentRect, that.mInParentRect);
    }

    @Override
    public int hashCode() {
        return Objects.hash(mOnScreenRect, mOnAppRect, mInParentRect);
    }

    @NonNull
    @Override
    public String toString() {
        return "AutoUIViewRect{" +
                "mOnScreenRect=" + (null != mOnScreenRect ? mOnScreenRect.toShortString(): "empty screen rect") +
                ", mOnAppRect=" + (null != mOnAppRect ? mOnAppRect.toShortString(): "empty app rect") +
                ", mInParentRect=" + (null != mInParentRect ? mInParentRect.toShortString() : "empty inParent rect") +
                '}';
    }
}
