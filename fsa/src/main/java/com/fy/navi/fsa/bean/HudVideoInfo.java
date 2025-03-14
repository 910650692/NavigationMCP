package com.fy.navi.fsa.bean;

public class HudVideoInfo {
    private int width;
    private int height;
    private int format;

    public HudVideoInfo() {
    }

    public HudVideoInfo(int width, int height, int format) {
        this.width = width;
        this.height = height;
        this.format = format;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public int getFormat() {
        return format;
    }

    public void setFormat(int format) {
        this.format = format;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    @Override
    public String toString() {
        return "HudVideoInfo{" +
                "width=" + width +
                ", height=" + height +
                ", format=" + format +
                '}';
    }
}
