package com.fy.navi.fsa.bean;

public class HudVideInfo {
    private int width;
    private int height;
    private int format;

    public HudVideInfo() {
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
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

    @Override
    public String toString() {
        return "HudVideInfo{" +
                "width=" + width +
                ", height=" + height +
                ", format=" + format +
                '}';
    }
}
