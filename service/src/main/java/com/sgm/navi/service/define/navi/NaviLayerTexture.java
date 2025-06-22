package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

import java.util.Arrays;

public class NaviLayerTexture {
    private int mResID;
    private byte[] mDataBuff;
    private int mAnchorType;
    private long mWidth;
    private long mHeight;
    private float mXRatio;
    private float mYRatio;
    private int mIconType;
    private boolean mIsGenMipmaps;
    private boolean mIsRepeat;
    private int mErrorCode;
    private String mName;
    private boolean mIsPreMulAlpha;

    public int getResID() {
        return mResID;
    }

    public void setResID(final int resID) {
        this.mResID = resID;
    }

    public byte[] getDataBuff() {
        return mDataBuff;
    }

    public void setDataBuff(final byte[] dataBuff) {
        this.mDataBuff = dataBuff;
    }

    public int getAnchorType() {
        return mAnchorType;
    }

    public void setAnchorType(final int anchorType) {
        this.mAnchorType = anchorType;
    }

    public long getWidth() {
        return mWidth;
    }

    public void setWidth(final long width) {
        this.mWidth = width;
    }

    public long getHeight() {
        return mHeight;
    }

    public void setHeight(final long height) {
        this.mHeight = height;
    }

    public float getXratio() {
        return mXRatio;
    }

    public void setXratio(final float xratio) {
        this.mXRatio = xratio;
    }

    public float getYratio() {
        return mYRatio;
    }

    public void setYratio(final float yratio) {
        this.mYRatio = yratio;
    }

    public int getIconType() {
        return mIconType;
    }

    public void setIconType(final int iconType) {
        this.mIconType = iconType;
    }

    public boolean isGenMipmaps() {
        return mIsGenMipmaps;
    }

    public void setGenMipmaps(final boolean genMipmaps) {
        mIsGenMipmaps = genMipmaps;
    }

    public boolean isRepeat() {
        return mIsRepeat;
    }

    public void setRepeat(final boolean repeat) {
        mIsRepeat = repeat;
    }

    public int getErrorCode() {
        return mErrorCode;
    }

    public void setErrorCode(final int errorCode) {
        this.mErrorCode = errorCode;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        this.mName = name;
    }

    public boolean isPreMulAlpha() {
        return mIsPreMulAlpha;
    }

    public void setPreMulAlpha(final boolean preMulAlpha) {
        mIsPreMulAlpha = preMulAlpha;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviLayerTexture{" +
                "resID=" + mResID +
                ", dataBuff=" + Arrays.toString(mDataBuff) +
                ", anchorType=" + mAnchorType +
                ", width=" + mWidth +
                ", height=" + mHeight +
                ", xRatio=" + mXRatio +
                ", yRatio=" + mYRatio +
                ", iconType=" + mIconType +
                ", isGenMipmaps=" + mIsGenMipmaps +
                ", isRepeat=" + mIsRepeat +
                ", errorCode=" + mErrorCode +
                ", name='" + mName + '\'' +
                ", isPreMulAlpha=" + mIsPreMulAlpha +
                '}';
    }
}
