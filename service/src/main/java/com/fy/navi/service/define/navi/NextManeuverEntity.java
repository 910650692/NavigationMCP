package com.fy.navi.service.define.navi;

import android.graphics.drawable.BitmapDrawable;

import androidx.annotation.NonNull;

public class NextManeuverEntity {
    private int mNextIconResource;
    private BitmapDrawable mNextIconDrawable;
    private boolean mIsNextManeuverVisible;
    private boolean mIsNextManeuverOffLine;
    private String mNextText;

    public int getNextIconResource() {
        return mNextIconResource;
    }

    public void setNextIconResource(int nextIconResource) {
        this.mNextIconResource = nextIconResource;
    }

    public BitmapDrawable getNextIconDrawable() {
        return mNextIconDrawable;
    }

    public void setNextIconDrawable(BitmapDrawable nextIconDrawable) {
        this.mNextIconDrawable = nextIconDrawable;
    }

    public boolean isNextManeuverVisible() {
        return mIsNextManeuverVisible;
    }

    public void setNextManeuverVisible(boolean nextManeuverVisible) {
        mIsNextManeuverVisible = nextManeuverVisible;
    }

    public boolean isNextManeuverOffLine() {
        return mIsNextManeuverOffLine;
    }

    public void setNextManeuverOffLine(boolean nextManeuverOffLine) {
        mIsNextManeuverOffLine = nextManeuverOffLine;
    }

    public String getNextText() {
        return mNextText;
    }

    public void setNextText(String nextText) {
        this.mNextText = nextText;
    }

    @NonNull
    @Override
    public String toString() {
        return "NextManeuverEntity{" +
                "mNextIconResource=" + mNextIconResource +
                ", mNextIconDrawable=" + mNextIconDrawable +
                ", mIsNextManeuverVisible=" + mIsNextManeuverVisible +
                ", mIsNextManeuverOffLine=" + mIsNextManeuverOffLine +
                ", nextText='" + mNextText + '\'' +
                '}';
    }
}
