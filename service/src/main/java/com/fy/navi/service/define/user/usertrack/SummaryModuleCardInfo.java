package com.fy.navi.service.define.user.usertrack;


public class SummaryModuleCardInfo {
    private int mMeasure;
    private String mName;
    private String mTitle;
    private String mUnit;

    private int mUnreadCityNum = 0;

    private String mText;
    private String mColor;
    private String mBgColor;

    public String getText() {
        return mText;
    }

    public void setText(final String text) {
        this.mText = text;
    }

    public String getColor() {
        return mColor;
    }

    public void setColor(final String color) {
        this.mColor = color;
    }

    public String getBgColor() {
        return mBgColor;
    }

    public void setBgColor(final String bgColor) {
        this.mBgColor = bgColor;
    }

    public int getUnreadCityNum() {
        return mUnreadCityNum;
    }

    public void setUnreadCityNum(final int unreadCityNum) {
        this.mUnreadCityNum = unreadCityNum;
    }

    public int getMeasure() {
        return mMeasure;
    }

    public void setMeasure(final int measure) {
        this.mMeasure = measure;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        this.mName = name;
    }

    public String getTitle() {
        return mTitle;
    }

    public void setTitle(final String title) {
        this.mTitle = title;
    }

    public String getUnit() {
        return mUnit;
    }

    public void setUnit(final String unit) {
        this.mUnit = unit;
    }
}
