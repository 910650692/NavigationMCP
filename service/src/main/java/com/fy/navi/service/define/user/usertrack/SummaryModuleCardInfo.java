package com.fy.navi.service.define.user.usertrack;

/**
 * @Description
 * @Author fh
 * @date 2024/12/27
 */
public class SummaryModuleCardInfo {
    public int measure;
    public String name;
    public String title;
    public String unit;

    public int unreadCityNum = 0;

    public String text;
    public String color;
    public String bgColor;

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public String getBgColor() {
        return bgColor;
    }

    public void setBgColor(String bgColor) {
        this.bgColor = bgColor;
    }

    public int getUnreadCityNum() {
        return unreadCityNum;
    }

    public void setUnreadCityNum(int unreadCityNum) {
        this.unreadCityNum = unreadCityNum;
    }

    public int getMeasure() {
        return measure;
    }

    public void setMeasure(int measure) {
        this.measure = measure;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }
}
