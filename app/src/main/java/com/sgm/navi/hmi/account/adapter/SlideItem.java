package com.sgm.navi.hmi.account.adapter;

public class SlideItem {
    private int mImageResId;
    private String mText;
    private String mDes;

    public SlideItem(final int imageResId, final String text, final String des) {
        this.mImageResId = imageResId;
        this.mText = text;
        this.mDes = des;
    }

    public int getImageResId() {
        return mImageResId;
    }

    public void setImageResId(final int imageResId) {
        this.mImageResId = imageResId;
    }

    public String getText() {
        return mText;
    }

    public void setText(final String text) {
        this.mText = text;
    }

    public String getDes() {
        return mDes;
    }

    public void setDes(final String des) {
        this.mDes = des;
    }
}

