package com.sgm.navi.scene.impl.navi.common;

import android.graphics.Bitmap;

import java.lang.ref.WeakReference;

/**
 * 场景中定义的背景资源信息
 * @author sgm
 * @version $Revision.*$
 */
public class AutoUIDrawable {
    private int mDayDrawableId = 0;
    private int mNightDrawableId = 0;
    private int mDayTintColorId = 0;
    private int mNightTintColorId = 0;
    private WeakReference<Bitmap> mBitmapWeakReference;

    public AutoUIDrawable(final int dayDrawableId, final int nightDrawableId,
                          final int dayTintColorId, final int nightTintColorId) {
        this.mDayDrawableId = dayDrawableId;
        this.mNightDrawableId = nightDrawableId;
        this.mDayTintColorId = dayTintColorId;
        this.mNightTintColorId = nightTintColorId;
    }

    public AutoUIDrawable(final int dayDrawableId) {
        this.mDayDrawableId = dayDrawableId;
    }

    public AutoUIDrawable(final Bitmap bitmap) {
        this.mDayDrawableId = 0;
        this.mNightDrawableId = 0;
        this.mDayTintColorId = 0;
        this.mNightTintColorId = 0;
        mBitmapWeakReference = new WeakReference<>(bitmap);
    }

    /**
     * @return the bitmap
     */
    public Bitmap getBitmap() {
        if (null != mBitmapWeakReference) {
            return mBitmapWeakReference.get();
        }
        return null;
    }

    /**
     * 释放资源
     */
    public void release() {
        if (null != mBitmapWeakReference) {
            final Bitmap bitmap = mBitmapWeakReference.get();
            if (null != bitmap && !bitmap.isRecycled()) {
                bitmap.recycle();
            }
            mBitmapWeakReference.clear();
            mBitmapWeakReference = null;
        }
    }

    /**
     * 获取白天资源id
     * @return the dayDrawableId
     */
    public int getDayDrawableId() {
        if (mDayDrawableId == 0) {
            return mNightDrawableId;
        }
        return mDayDrawableId;
    }

    /**
     * 获取黑夜资源ID
     * @return the nightDrawableId
     */
    public int getNightDrawableId() {
        if (mNightDrawableId == 0) {
            return mDayDrawableId;
        }
        return mNightDrawableId;
    }

    /**
     * @return the dayTintColorId
     */
    public int getDayTintColorId() {
        if (mDayTintColorId == 0) {
            return mNightTintColorId;
        }
        return mDayTintColorId;
    }

    /**
     * @return the nightTintColorId
     */
    public int getNightTintColorId() {
        if (mNightTintColorId == 0) {
            return mDayTintColorId;
        }
        return mNightTintColorId;
    }
}
