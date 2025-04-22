package com.fy.navi.fsa.bean;

/**
 * Hud视频流init
 */
public class HudVideInfo {
    /**
     * width	int	每帧图片的宽
     */
    private int width;
    /**
     * height	int	每帧图片的高
     */
    private int height;
    /**
     * format	int	图片格式：
     * 1、RGBA_8888
     * 2、RGBX_8888
     * 3、RGB_888
     * 4、RGB_565
     * 5、RGBA_5551
     * 6、RGBA_4444
     */
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
