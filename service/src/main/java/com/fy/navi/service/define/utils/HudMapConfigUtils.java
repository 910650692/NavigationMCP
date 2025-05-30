package com.fy.navi.service.define.utils;

/**
 * HUD 地图参数配置工具类
 */
public class HudMapConfigUtils {

    // 默认的 HUD 地图参数
    private static final long DEFAULT_X = 0;
    private static final long DEFAULT_Y = 0;
    private static final long DEFAULT_WIDTH = 328;
    private static final long DEFAULT_HEIGHT = 172;
    private static final long DEFAULT_SCREEN_WIDTH = 328;
    private static final long DEFAULT_SCREEN_HEIGHT = 172;

    /**
     * 获取 X 坐标
     */
    public static long getX() {
        return DEFAULT_X;
    }

    /**
     * 获取 Y 坐标
     */
    public static long getY() {
        return DEFAULT_Y;
    }

    /**
     * 获取宽度
     */
    public static long getWidth() {
        return DEFAULT_WIDTH;
    }

    /**
     * 获取高度
     */
    public static long getHeight() {
        return DEFAULT_HEIGHT;
    }

    /**
     * 获取屏幕宽度
     */
    public static long getScreenWidth() {
        return DEFAULT_SCREEN_WIDTH;
    }

    /**
     * 获取屏幕高度
     */
    public static long getScreenHeight() {
        return DEFAULT_SCREEN_HEIGHT;
    }
}
