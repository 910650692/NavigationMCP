package com.fy.navi.hmi.utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringUtils {
    private static final double KB = 1024;
    private static final double MB = 1048576;
    private static final double GB = 1073741824;

    /**
     * HH:mm 格式
     */
    private static final Pattern TIME_PATTERN = Pattern.compile(
            "^(?:[01]\\d|2[0-3]):[0-5]\\d$"
    );

    /**
     * yyyy-MM-dd HH:mm:ss 格式
     */
    private static final Pattern DATETIME_PATTERN = Pattern.compile(
            "^\\d{4}-(?:0[1-9]|1[0-2])-(?:0[1-9]|[12]\\d|3[01]) (?:[01]\\d|2[0-3]):[0-5]\\d:[0-5]\\d$"
    );

    /**
     * Convert byte value to normal data size.
     *
     * @param byteSize byte size
     * @return size description
     */
    public static String byteToString(long byteSize) {
        StringBuilder bytes = new StringBuilder();
        DecimalFormat format = new DecimalFormat("###.0");
        if (byteSize >= GB) {
            double size = byteSize / GB;
            bytes.append(format.format(size)).append("G");
        } else if (byteSize >= MB) {
            double size = byteSize / MB;
            bytes.append(format.format(size)).append("M");
        } else if (byteSize >= KB) {
            double size = byteSize / KB;
            bytes.append(format.format(size)).append("K");
        } else {
            if (byteSize <= 0) {
                bytes.append("0B");
            } else {
                bytes.append((int) byteSize).append("B");
            }
        }
        return bytes.toString();
    }

    /**
     * 辅助方法：将字节数转换为GB、MB、KB或B的字符串表示
     *
     * @param byteCount
     * @return
     */
    public static String formatSize(BigInteger byteCount) {
        BigDecimal sizeInGB = new BigDecimal(byteCount).divide(new BigDecimal(1024 * 1024 * 1024), 2, RoundingMode.HALF_UP);
        if (sizeInGB.compareTo(BigDecimal.ZERO) > 1) {
            return sizeInGB.toPlainString() + "GB";
        }
        BigDecimal sizeInMB = new BigDecimal(byteCount).divide(new BigDecimal(1024 * 1024), 2, RoundingMode.HALF_UP);
        if (sizeInMB.compareTo(BigDecimal.ZERO) > 0) {
            return sizeInMB.toPlainString() + "MB";
        }
        BigDecimal sizeInKB = new BigDecimal(byteCount).divide(new BigDecimal(1024), 2, RoundingMode.HALF_UP);
        if (sizeInKB.compareTo(BigDecimal.ZERO) > 0) {
            return sizeInKB.toPlainString() + "KB";
        }
        // 如果都不满足，则返回字节数
        return byteCount.toString() + "B";
    }

    /**
     * 辅助方法：将字节数转换为GB、MB、KB或B的字符串表示
     *
     * @param bytes
     * @return
     */
    public static String formatSize(long bytes) {
        DecimalFormat decimalFormat = new DecimalFormat("#0.0");
        if (bytes / 1024.0 / 1024.0 / 1024.0 < 1) { //数据大小小于1G
            return decimalFormat.format(bytes / 1024.0 / 1024.0) + "MB";
        } else {
            return decimalFormat.format(bytes / 1024.0 / 1024.0 / 1024.0) + "GB";
        }
    }

    /**
     * 判断时间字符串是否符合HH:mm格式
     *
     * @param str
     * @return
     */
    public static boolean isHHmmFormat(String str) {
        if (str == null || str.isEmpty()) {
            return false;
        }
        Matcher matcher = TIME_PATTERN.matcher(str);
        return matcher.matches();
    }

    /**
     * 判断时间字符串是否符合yyyy-MM-dd HH:mm:ss 格式
     *
     * @param str
     * @return
     */
    public static boolean isDatetimeByRegex(String str) {
        if (str == null || str.isEmpty()) {
            return false;
        }
        Matcher matcher = DATETIME_PATTERN.matcher(str);
        return matcher.matches();
    }
}
