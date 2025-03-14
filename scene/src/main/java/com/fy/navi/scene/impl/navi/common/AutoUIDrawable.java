package com.fy.navi.scene.impl.navi.common;

import android.graphics.Bitmap;

import java.lang.ref.WeakReference;

/**
 * 场景中定义的背景资源信息
 */
public class AutoUIDrawable {
    private int mDayDrawableId = 0;
    private int mNightDrawableId = 0;
    private int mDayTintColorId = 0;
    private int mNightTintColorId = 0;
    private WeakReference<Bitmap> bitmapWeakReference;

    public AutoUIDrawable(int dayDrawableId, int nightDrawableId, int dayTintColorId, int nightTintColorId) {
        this.mDayDrawableId = dayDrawableId;
        this.mNightDrawableId = nightDrawableId;
        this.mDayTintColorId = dayTintColorId;
        this.mNightTintColorId = nightTintColorId;
    }

    public AutoUIDrawable(int dayDrawableId) {
        this.mDayDrawableId = dayDrawableId;
    }

    public AutoUIDrawable(Bitmap bitmap) {
        this.mDayDrawableId = 0;
        this.mNightDrawableId = 0;
        this.mDayTintColorId = 0;
        this.mNightTintColorId = 0;
        bitmapWeakReference = new WeakReference<>(bitmap);
    }

    public Bitmap getBitmap() {
        if (null != bitmapWeakReference) {
            return bitmapWeakReference.get();
        }
        return null;
    }

    public void release() {
        if (null != bitmapWeakReference) {
            Bitmap bitmap = bitmapWeakReference.get();
            if (null != bitmap && !bitmap.isRecycled()) {
                bitmap.recycle();
            }
            bitmapWeakReference.clear();
            bitmapWeakReference = null;
        }
    }

    public int getDayDrawableId() {
        if (mDayDrawableId == 0) {
            return mNightDrawableId;
        }
        return mDayDrawableId;
    }

    public int getNightDrawableId() {
        if (mNightDrawableId == 0) {
            return mDayDrawableId;
        }
        return mNightDrawableId;
    }

    public int getDayTintColorId() {
        if (mDayTintColorId == 0) {
            return mNightTintColorId;
        }
        return mDayTintColorId;
    }

    public int getNightTintColorId() {
        if (mNightTintColorId == 0) {
            return mDayTintColorId;
        }
        return mNightTintColorId;
    }
}
