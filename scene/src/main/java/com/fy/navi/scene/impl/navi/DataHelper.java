package com.fy.navi.scene.impl.navi;

import android.annotation.SuppressLint;

import com.android.utils.log.Logger;

/**
 * 数据处理辅助类
 * @author fy
 * @version $Revision.*$
 */
public final class DataHelper {

    private DataHelper() {

    }

    public static final String TAG = "DataHelper";

    public static final String KILOMETER = "公里";
    public static final String MINUTE = "分钟";

    /**
     * 数据转换 >100公里保留整数，<100公里保留小数点后一位
     * @param meters 长度 米
     * @return string
     */
    @SuppressLint("DefaultLocale")
    public static String convertMetersToKilometers(final int meters) {
        final double kilometers = meters / 1000.0;
        if (kilometers < 100) {
            // 保留小数点后一位
            return String.format("%.1f" + KILOMETER, kilometers);
        } else {
            // 保留整数部分
            return String.format("%d" + KILOMETER, (int) kilometers);
        }
    }

    /**
     * 将秒转换为更详细的分钟格式的字符串。
     *
     * @param seconds 秒数
     * @return 表示分钟的详细字符串
     */
    public static String convertSecondsToDetailedMinutes(final long seconds) {
        final long minutes = seconds / 60;
        return minutes + MINUTE;
    }

    /**
     * @param number 数据
     * @param n 右移量
     * @return 第n位的值
     */
    public static int getNthBit(final long number, final int n) {
        Logger.i(TAG, "getNthBit number:" + number + " n:" + n);
        // 使用位运算右移n位，然后与1进行与运算，得到第n位的值
        return (int)((number >> n) & 1);
    }
}
