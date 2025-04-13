package com.fy.navi.service.define.navi;

import androidx.annotation.NonNull;

public class SoundInfoEntity {
    private String mText;//播报文本
    private int mRingType;//叮咚音类型
    private boolean mIsRingType = false;//是否播报叮咚音

    private boolean highPriority = false;

    private int soundType; // 播报类型

    public String getText() {
        return mText;
    }

    public void setText(final String text) {
        this.mText = text;
    }

    public int getRingType() {
        return mRingType;
    }

    public void setRingType(final int ringType) {
        this.mRingType = ringType;
    }

    public boolean isRingType() {
        return mIsRingType;
    }

    public void setRingType(final boolean ringType) {
        mIsRingType = ringType;
    }

    public int getSoundType() {
        return soundType;
    }

    public void setSoundType(@PlayModule.PlayModule1 int soundType) {
        this.soundType = soundType;
    }

    public boolean isHighPriority() {
        return highPriority;
    }

    public void setHighPriority(boolean highPriority) {
        this.highPriority = highPriority;
    }

    @Override
    public String toString() {
        return "SoundInfoEntity{" +
                "mText='" + mText + '\'' +
                ", mRingType=" + mRingType +
                ", mIsRingType=" + mIsRingType +
                ", highPriority=" + highPriority +
                ", soundType=" + soundType +
                '}';
    }
}
