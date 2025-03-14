package com.fy.navi.hmi.account.adapter;

public class SlideItem {
    private int imageResId;
    private String text;
    private String des;

    public SlideItem(int imageResId, String text, String des) {
        this.imageResId = imageResId;
        this.text = text;
        this.des = des;
    }

    public int getImageResId() {
        return imageResId;
    }

    public void setImageResId(int imageResId) {
        this.imageResId = imageResId;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getDes() {
        return des;
    }

    public void setDes(String des) {
        this.des = des;
    }
}

