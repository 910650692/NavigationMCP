package com.android.utils;

import android.content.Context;
import android.text.Html;
import android.text.Spanned;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * String工具类
 *
 * @author wangyunsheng
 * @date 2012-12-10
 */
public class StringUtils {
    private static final String TAG = "StringUtils";

    public static final String[] unitDistArray = new String[]{"m", "km", "米", "公里"};

    public static final String[] unitTimeArray = new String[]{"m", "h", "分钟", "小时"};
    public static final String[] unitTimeArray2 = new String[]{"m", "h", "d", "分钟", "小时", "天"};

    public static final String lessOneMinute = "少于1分钟";

    public static final String HTML_TIME_HOUR = "<font color=\"#ffffff\">%d</font><font color=\"#b5b7b6\">%s</font>";

    public static final String HTML_TIME_MINUTE = "<font color=\"#ffffff\">%d</font><font color=\"#b5b7b6\">%s</font>";

    public static final String HTML_TIME_HOUR_MINUTE =
            "<font color=\"#ffffff\">%d</font><font color=\"#b5b7b6\">%s</font><font color=\"#ffffff\">%d</font><font"
                    + " color=\"#b5b7b6\">%s</font>";

    private static final String HTML_DIST = "<font color=\"#ffffff\">%d</font><font color=\"#b5b7b6\">%s</font>";

    private static final String HTML_DIST_NO_DECIMAL =
            "<font color=\"#ffffff\">%.0f</font><font color=\"#b5b7b6\">%s</font>";

    private static final String HTML_DIST_WITH_DECIMAL =
            "<font color=\"#ffffff\">%.1f</font><font color=\"#b5b7b6\">%s</font>";

    private static final String HTML_DIST_TIME = "<font color=\"#b5b7b6\">剩:</font>%s&nbsp;&nbsp;%s";

    private static final String UNIT_MINUTE = "分";
    private static final String UNIT_MINUTE_FOR_31 = "分钟";

    public static final int TIME_EN_UNIT = -1; // 英文长度单位开始的下标
    public static final int TIME_ZH_UNIT = 2; // 中文长度单位开始的下标

    public static final int SECOND_OF_MINUTE = 60; // 一分钟的秒数
    public static final int SECOND_OF_HOUR = 3600; // 一小时的秒数
    public static final int SECOND_OF_DAY = 86400; // 一天的秒数

    /**
     * 距离中英文单位枚举
     */
    public enum UnitLangEnum {

        EN(0), ZH(1);

        private int nUnit;

        private UnitLangEnum(int nUnit) {
            this.nUnit = nUnit;
        }

        public int getnUnit() {
            return nUnit;
        }

        public void setnUnit(int nUnit) {
            this.nUnit = nUnit;
        }

    }

    public static void formatDistAndTime(int nDist, int nTime, StringBuffer formatDistTime) {
        if (formatDistTime == null) {
            return;
        }
        StringBuffer dist = new StringBuffer();
        StringBuffer time = new StringBuffer();

        formatHtmlDistance(nDist, dist);

        formatHtmlTime(nTime, time);

        formatDistTime.append(String.format(HTML_DIST_TIME, dist, time));

        return;
    }

    /**
     * 转换剩余距离为分档显示距离
     *
     * @param nDist ，剩余距离，单位：米（m）
     * @return 返回转换之后的距离值，为整零值
     */
    public static String formatDistanceToString(int nDist, UnitLangEnum langEnum) {
        StringBuilder formatDist = new StringBuilder();
        formatDistance(nDist, langEnum, formatDist);

        return formatDist.toString();
    }

    /**
     * 转换剩余距离为分档显示距离
     *
     * @param nDist ，剩余距离，单位：米（m）
     * @return String数组，索引0为值；1为单位
     */
    public static String[] formatDistance(int nDist, UnitLangEnum langEnum) {
        String[] data = new String[2];
        int offset = 0;
        boolean bNoZero = false;
        int nUnit = langEnum.getnUnit();
        // 偏移到中文单位索引
        if (nUnit != 0) {
            nUnit++;
        }
        if (nDist >= 1000) {
            offset = 1;
            // Log.d(TAG, "++ formatDistance   nDist="+nDist);
            if (nDist % 1000 == 0) {
                bNoZero = true;
            }
            if (bNoZero) {
                data[0] = String.format("%.0f", (double) nDist / 1000);
                data[1] = unitDistArray[nUnit + offset];
            } else {
                data[0] = String.format("%.1f", (double) nDist / 1000);
                data[1] = unitDistArray[nUnit + offset];
            }
        } else {
            offset = 0;
            data[0] = String.format("%d", nDist);
            data[1] = unitDistArray[nUnit + offset];
        }
        return data;
    }

    /**
     * 转换剩余距离为分档显示距离
     *
     * @param nDist ，剩余距离，单位：米（m）
     * @return 返回转换之后的距离值，为整零值
     */
    public static void formatDistance(int nDist, UnitLangEnum langEnum, StringBuilder formatDist) {

        int offset = 0;
        boolean bNoZero = false;
        int nUnit = langEnum.getnUnit();

        // 偏移到中文单位索引
        if (nUnit != 0) {
            nUnit++;
        }
        if (nDist >= 1000) {
            offset = 1;

            String distFormat = "";
            if (formatDist != null) {
                int km = nDist / 1000;
                if (km >= 100) {
                    // 大于等于100km
                    distFormat = "%d%s";
                    formatDist.append(String.format(distFormat, km, unitDistArray[nUnit + offset]));
                } else {
                    formatDist.append(BigDecimal.valueOf((double) nDist / 1000).setScale(nDist % 1000 == 0 ? 0 : 1, BigDecimal.ROUND_DOWN).toString()).append(unitDistArray[nUnit + offset]);
                }
            }
        } else {
            offset = 0;
            if (formatDist != null) {
                formatDist.append(String.format("%d%s", nDist, unitDistArray[nUnit + offset]));
            }
        }
    }

    public static void formatHtmlDistance(int nDist, StringBuffer formatDist) {

        int offset = 0;
        boolean bNoZero = false;

        if (nDist >= 1000) {
            offset = 1;
            // Log.d(TAG, "++ formatDistance   nDist="+nDist);

            if (nDist % 1000 == 0) {
                bNoZero = true;
            }

            String distFormat = "";
            if (bNoZero) {
                distFormat = HTML_DIST_NO_DECIMAL;
            } else {
                distFormat = HTML_DIST_WITH_DECIMAL;
            }

            if (formatDist != null && offset < unitDistArray.length) {
                int km = nDist / 1000;
                if (km >= 100) {
                    // 大于等于100km
                    distFormat = HTML_DIST;
                    formatDist.append(String.format(distFormat, km, unitDistArray[offset]));
                } else {
                    formatDist.append(String.format(distFormat, (double) nDist / 1000,
                            unitDistArray[offset]));
                }
            }
        } else {
            offset = 0;
            if (formatDist != null && offset < unitDistArray.length) {
                formatDist.append(String.format(HTML_DIST, nDist, unitDistArray[offset]));
            }
        }
    }


    /**
     * 转换时间为显示时间
     *
     * @param nTime      ，剩余距离，单位：秒（s）
     * @param formatTime ，格式化之后的字符串输出，包含单位后缀
     * @return 返回转换之后的距离值，为整零值
     */
    public static void formatTime(int nTime, UnitLangEnum langEnum, StringBuffer formatTime) {

        int offset = 0;
        boolean bNoZero = false;
        int nUnit = langEnum.getnUnit();

        // 偏移到中文单位索引
        if (nUnit != 0) {
            nUnit++;
        }

        if (nTime >= 3600) {
            offset = 1;
            // 判断小数点后一位是0
            if (nTime % 3600 >= 0 && nTime % 3600 < 360) {
                bNoZero = false;
            }

            if (formatTime != null) {
                if (bNoZero) {
                    formatTime.append(String.format("%.1f%s", (double) nTime / 3600, unitTimeArray[nUnit + offset]));
                } else {
                    formatTime.append(String.format("%.0f%s", (double) nTime / 3600, unitTimeArray[nUnit + offset]));
                }
            }
        } else {
            offset = 0;
            if (nTime < 60) {
                if (formatTime != null) {
                    formatTime.append(lessOneMinute);
                }
            } else {
                if (formatTime != null) {
                    formatTime.append(String.format("%d%s", nTime / 60, unitTimeArray[nUnit + offset]));
                }
            }
        }
        return;
    }

    /**
     * 转换时间为显示时间
     *
     * @param nTime      ，剩余距离，单位：秒（s）
     * @param formatTime ，格式化之后的字符串输出，包含单位后缀
     * @return 返回转换之后的距离值，为整零值
     */
    public static void formatHtmlTime(int nTime, StringBuffer formatTime) {

        int offset = 0;

        int nHour = -1;
        int nMinute = -1;
        if (nTime >= 3600) {
            offset = 1;

            nHour = nTime / 3600;
            nMinute = (nTime / 60) % 60;

            if (formatTime != null && offset >= 0 && offset < unitTimeArray.length) {
                if (nMinute > 0) {
                    formatTime.append(String.format(HTML_TIME_HOUR_MINUTE, nHour,
                            unitTimeArray[offset], nMinute, unitTimeArray[offset - 1]));
                } else {
                    formatTime.append(String.format(HTML_TIME_HOUR, nHour, unitTimeArray[offset]));
                }
            }
        } else {
            offset = 0;

            nMinute = nTime / 60;

            if (nTime < 60) {
                if (formatTime != null) {
                    formatTime.append(lessOneMinute);
                }
            } else {
                if (formatTime != null) {
                    formatTime.append(String.format(HTML_TIME_MINUTE, nMinute,
                            unitTimeArray[offset]));
                }
            }
        }
        return;
    }

    /**
     * 计算两点球面距离
     *
     * @param sx 起点X坐标
     * @param sy 起点Y坐标
     * @param ex 终点X坐标
     * @param ey 终点Y坐标
     * @return
     */
    public static double geoSphereDistance(double sx, double sy, double ex, double ey) {
        double dDist = 0;
        double x1 = sx;
        double x2 = ex;
        double y1 = sy;
        double y2 = ey;
        double dx = (x1 - x2);
        double dy = (y1 - y2);
        double l = ((y1 + y2) / 2);

        double angle = l;
        angle = angle * (3.141592653589793 / 100000 / 180);
        dx = dx * Math.cos(angle);
        dx = dx * dx;
        dy = dy * dy;
        dDist = Math.sqrt((dx + dy) * 1.1119104);
        return dDist;
    }

    /**
     * 创建文件夹
     *
     * @param path
     */
    public static void createDir(final String path) {
        if (TextUtils.isEmpty(path)) {
            return;
        }
        File file = new File(path);
        if (!file.exists()) {
            file.mkdirs();
        }
    }

    /**
     * 将毫秒数换算成x天x时x分x秒
     *
     * @param time 时长
     * @return x天x时x分x秒
     */
    public static String getFormatTime(long time) {
        long day1 = time / (24 * 3600);
        long hour1 = time % (24 * 3600) / 3600;
        long minute1 = time % 3600 / 60;
        long second1 = time % 60;
        String hms = String.format("%s%s%s%s", day1 < 1 ? "" : day1 + "天", hour1 < 1 ? "" : hour1
                + "小时", minute1 < 1 ? "" : minute1 + "分", second1 < 1 ? "" : second1 + "秒");
        return hms;
    }

    /**
     * 返回类型：xxx分钟|xxx小时xxx分钟|xxx天xxx小时
     *
     * @param nTime
     * @param nUnit
     * @param formatTime
     */
    public static void formatTime2(long nTime, int nUnit, StringBuffer formatTime) {
        if (formatTime != null) {
            if (nTime < SECOND_OF_MINUTE) {
                formatTime.append(lessOneMinute);
            } else {
                int day = 0;
                int hour = 0;
                int minute = 0;
                hour = (int) ((nTime / SECOND_OF_HOUR) % 24);
                minute = (int) ((nTime / SECOND_OF_MINUTE) % 60);

                if (nTime < SECOND_OF_HOUR) {
                    // xxx分钟
                    formatTime.append(minute).append(unitTimeArray2[nUnit + 1]);
                } else if (nTime < SECOND_OF_DAY) {
                    // xxx小时xxx分
                    formatTime.append(hour).append(unitTimeArray2[nUnit + 2]);
                    if (minute > 0) {
                        formatTime.append(minute).append(UNIT_MINUTE);
                    }
                } else {
                    // xxx天xxx小时
                    day = (int) (nTime / SECOND_OF_DAY);
                    formatTime.append(day).append(unitTimeArray2[nUnit + 3]);
                    if (hour > 0) {
                        formatTime.append(hour).append(unitTimeArray2[nUnit + 2]);
                    }
                }
            }

        }
    }

    /**
     * 返回类型：xxx分钟|xxx小时xxx分钟
     *
     * @param nTime
     * @param nUnit Spanned
     */
    public static Spanned formatTimeForEp31(int nTime, int nUnit) {
        if (nTime < SECOND_OF_MINUTE) {
            return Html.fromHtml("少于<font color='#000000'>" + 1 + "</font> 分钟");
        } else {
            int day = 0;
            int hour = 0;
            int minute = 0;
            hour = (nTime / SECOND_OF_HOUR) % 24;
            minute = (nTime / SECOND_OF_MINUTE) % 60;

            if (nTime < SECOND_OF_HOUR) {
                // xxx分钟
                return Html.fromHtml("<font color='#000000'>" + minute + "</font> " + unitTimeArray2[nUnit + 1]);
            } else {//if (nTime < SECOND_OF_DAY)
                // xxx小时xxx分
                if (minute > 0) {
                    return Html.fromHtml("<font color='#000000'>" + hour + "</font> " + unitTimeArray2[nUnit + 2] +
                            "<font color='#000000'>" + minute + "</font> " + unitTimeArray2[nUnit + 1]);
                } else {
                    return Html.fromHtml("<font color='#000000'>" + hour + "</font> " + unitTimeArray2[nUnit + 2]);
                }
            }
        }
    }

    /**
     * 小数取整 四舍五入
     *
     * @param src
     * @return
     */
    public static String strRound(@NonNull String src) {
        if (src != null && src.contains(".")) {
            try {
                return String.valueOf(Math.round(Float.parseFloat(src)));
            } catch (Exception ignore) {
            }
        }
        return src;
    }

    public static String formatTime2(long nTime, int nUnit) {
        StringBuffer buffer = new StringBuffer();
        formatTime2(nTime, nUnit, buffer);
        return buffer.toString();
    }

    public static boolean isNotEmpty(String s) {
        if (s == null || s.length() == 0) {
            return false;
        } else {
            return true;
        }
    }

    public static boolean isEmpty(String s) {
        if (s == null || s.length() == 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * 利用正则表达式判断字符串是否是数字
     *
     * @param str
     * @return
     */
    public static boolean isNumeric(String str) {
        if (isEmpty(str)) {
            return false;
        }
        Pattern pattern = Pattern.compile("[0-9]*");
        Matcher isNum = pattern.matcher(str);
        if (!isNum.matches()) {
            return false;
        }
        return true;
    }

    /**
     * 判断是否包含中文、英文和数字
     *
     * @param str 字符串
     * @return 包含中文、英文和数字返回 true；否则返回 false
     */
    public static boolean containChineseEnglishNumber(String str) {
        Pattern pattern = Pattern.compile("^.*[\\u4e00-\\u9fa5a-zA-Z0-9]+.*$");
        Matcher matcher = pattern.matcher(str);
        return matcher.matches();
    }

    public static String byteSizeToString(int nSize) {
        try {
            java.text.DecimalFormat df = new java.text.DecimalFormat();
            String strSize;
            if (nSize < 1024) {
                strSize = nSize + "B";
            } else if (nSize < 1048576) { // 1024*1024
                df.applyPattern("0");
                double d = (double) nSize / 1024;
                strSize = df.format(d) + "K";
            } else if (nSize < 1073741824) { // 1024*1024*1024
                df.applyPattern("0.0");
                double d = (double) nSize / 1048576;
                strSize = df.format(d) + "M";
            } else { // >1G
                df.applyPattern("0.0");
                double d = (double) nSize / 1073741824;
                strSize = df.format(d) + "G";
            }
            return strSize;
        } catch (Exception e) {
            return "0";
        }
    }

    /**
     * 将bytes转换为大小,
     *
     * @return
     */
    public static String byteSizeToStringForLong(Long nSize) {
        java.text.DecimalFormat df = new java.text.DecimalFormat();
        String strSize;
        if (nSize < 1024) {
            strSize = nSize + "B";
        } else if (nSize < 1048576) { // 1024*1024
            df.applyPattern("0");
            double d = nSize / 1024.0;
            strSize = df.format(d) + "K";
        } else if (nSize < 1073741824) { // 1024*1024*1024
            df.applyPattern("0.0");
            double d = nSize / 1048576.0;
            strSize = df.format(d) + "M";
        } else { // >1G
            df.applyPattern("0.0");
            double d = nSize / 1073741824.0;
            strSize = df.format(d) + "G";
        }
        return strSize;
    }

    /**
     * 处理poi结果中的转义字符
     *
     * @param str 输入字符串
     * @return 返回处理了转义字符后的字符串
     */
    public static String trimString(String str) {
        if (str == null) {
            return null;
        }

        String trimStr = str.toLowerCase(Locale.getDefault()).replace("\\t", "&#x0009;").replace("\\r", "&#x000d;")
                .replace("\\n", "&#x000a;");

        // 需要连续两次转义
        trimStr = Html.fromHtml(trimStr).toString(); // 把&转义出来CarServic
        trimStr = Html.fromHtml(trimStr).toString(); // 把其它字符转义出来

        return trimStr.toUpperCase();
    }

    public static String getUrlDecodeString(String str) {
        String ret = null;
        try {
            ret = URLDecoder.decode(str, "utf-8");
        } catch (UnsupportedEncodingException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return ret;
    }

    public static String getUrlEncodeString(String str) {
        String ret = null;
        try {
            ret = URLEncoder.encode(str, "utf-8");
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }

        return ret;
    }

    public static String getPoiAddress(String originAddress) {
        String retResult = "";
        if (!TextUtils.isEmpty(originAddress)) {
            if (!originAddress.endsWith("附近")) {
                retResult = "在" + originAddress + "附近";
            } else {
                retResult = "在" + originAddress;
            }
        }
        return retResult;
    }

    public static String getDirection(double dx, double dy) {
        if (dx == 0) {
            if (dy == 0) {
                return "无";
            }

            if (dy > 0) {
                return "北";
            }

            if (dy < 0) {
                return "南";
            }
            return "无";
        }

        double angle = Math.atan(dy / dx);
        String dir = "";
        if (dx > 0) {
            if (angle < -Math.PI * 3 / 8) {
                dir = "南";
            } else if (angle < -Math.PI / 8) {
                dir = "东南";
            } else if (angle < Math.PI / 8) {
                dir = "东";
            } else if (angle < Math.PI * 3 / 8) {
                dir = "东北";
            } else {
                dir = "北";
            }
        } else { // dx < 0
            if (angle < -Math.PI * 3 / 8) {
                dir = "北";
            } else if (angle < -Math.PI / 8) {
                dir = "西北";
            } else if (angle < Math.PI / 8) {
                dir = "西";
            } else if (angle < Math.PI * 3 / 8) {
                dir = "西南";
            } else {
                dir = "南";
            }
        }

        return dir;
    }

    /**
     * 将阿拉伯数字转为汉字说法
     *
     * @param number >= 0，目前最多只是到万单位
     * @return
     */
    public static String numberToChineseWord(int number) {
        String wd = "";

        if (number < 0) {
            return wd;
        } else if (number < 10) {
            return singleNumberToChineseWord(number);
        } else if (number < 100) {
            return doubleNumberToChineseWord(number);
        }

        int num = number % (100000);
        if (num >= 10000) {
            wd += singleNumberToChineseWord(num / 10000) + "万";
        }
        num = number % 10000;
        if (num >= 1000) {
            wd += singleNumberToChineseWord(num / 1000) + "千";
        }
        num = number % 1000;
        if (num >= 100) {
            wd += singleNumberToChineseWord(num / 100) + "百";
        }
        num = number % 100;
        if (num >= 10) {
            wd += singleNumberToChineseWord(num / 10) + "十";
        }
        num = number % 10;
        if (num >= 1) {
            wd += singleNumberToChineseWord(num);
        }
        return wd;
    }

    /**
     * 将单个数字转为汉字表示
     *
     * @param number
     * @return
     */
    public static String singleNumberToChineseWord(int number) {
        number %= 10;
        String wd = "";
        switch (number) {
            case 0:
                wd = "零";
                break;
            case 1:
                wd = "一";
                break;
            case 2:
                wd = "二";
                break;
            case 3:
                wd = "三";
                break;
            case 4:
                wd = "四";
                break;
            case 5:
                wd = "五";
                break;
            case 6:
                wd = "六";
                break;
            case 7:
                wd = "七";
                break;
            case 8:
                wd = "八";
                break;
            case 9:
                wd = "九";
                break;
            default:
                break;
        }

        return wd;
    }

    /**
     * 周几数字显示
     *
     * @param number
     * @return
     */
    public static String singleNumberForWeek(int number) {
        number %= 10;
        String wd = "";
        switch (number) {
            case 0:
                wd = "日";
                break;
            case 1:
                wd = "一";
                break;
            case 2:
                wd = "二";
                break;
            case 3:
                wd = "三";
                break;
            case 4:
                wd = "四";
                break;
            case 5:
                wd = "五";
                break;
            case 6:
                wd = "六";
                break;
            default:
                Logger.i(TAG, " singleNumberForWeek number:" , number);
                break;
        }

        return wd;
    }

    /**
     * 0-99数字转为汉字表示
     *
     * @param number 1或2位数字
     * @return
     */
    public static String doubleNumberToChineseWord(int number) {
        number %= 100;

        if (number < 10) {
            return singleNumberToChineseWord(number);
        }
        if (number == 10) {
            return "十";
        }

        StringBuffer sb = new StringBuffer();
        if (number >= 20) {
            sb.append(singleNumberToChineseWord(number / 10));
        }
        sb.append("十");

        if ((number % 10) != 0) {
            sb.append(singleNumberToChineseWord(number % 10));
        }

        return sb.toString();
    }

    /**
     * ，剩余距离，单位：米（m）， 不超过10万
     *
     * @param dist
     * @return 例如： 345公里
     */
    public static String getDistance(long dist) {
        if (dist <= 0) {
            return Html.fromHtml("<font color='black'>" + 0 + "</font>") + "米";
//            return 0 + "米";
        } else if (dist < 1000) {
            return dist + "米";
        } else if (dist < 100000) {
            int k = (int) (dist / 1000);
            int m = (int) ((dist % 1000) / 100);
            return k + "." + m + "公里";
        } else {
            return dist / 1000 + "公里";
        }
    }

    public static Spanned getDistanceForEp31(int dist) {
        if (dist <= 0) {
            return Html.fromHtml("<font color='#000000'>" + 0 + "</font> 米");
        } else if (dist < 1000) {
            return Html.fromHtml("<font color='#000000'>" + dist + "</font> 米");
        } else if (dist < 100000) {
            int k = dist / 1000;
            int m = (dist % 1000) / 100;
            return Html.fromHtml("<font color='#000000'>" + k + "." + m + "</font> 公里");
        } else {
            return Html.fromHtml("<font color='#000000'>" + dist / 1000 + "</font> 公里");
        }
    }

    /**
     * 获取距离（单位为米）
     */
    public static int getDistanceFilterUnit(String distance) {
        if (null == distance) {
            return 0;
        }
        if (distance.contains("公里")) {
            return (int) (Double.parseDouble(distance.replace("公里", "")) * 1000);
        } else if (distance.contains("米")) {
            return (int) (Double.parseDouble(distance.replace("米", "")));
        } else if (distance.contains("km")) {
            return (int) (Double.parseDouble(distance.replace("km", "")) * 1000);
        } else if (distance.contains("m")) {
            return (int) (Double.parseDouble(distance.replace("m", "")));
        }
        return 0;
    }

    /**
     * 转换距离为汉字表示的分档显示距离
     *
     * @param nDist ，剩余距离，单位：米（m）， 不超过10万
     * @return 返回转换之后的距离值，为整零值,米/公里 ，最多一个小数位
     */
    public static String formatDistanceToChineseString(int nDist) {
        if (nDist <= 0) {
            return "未知";
        }
        if (nDist >= 100000) {
            return numberToChineseWord(nDist / 1000) + "公里";
        } else if (nDist < 1000) {
            return numberToChineseWord(nDist) + "米";
        } else {
            int lit = (nDist % 1000) / 100;
            nDist /= 1000;
            if (lit == 0) {
                return numberToChineseWord(nDist) + "公里";
            } else {
                return numberToChineseWord(nDist) + "点" + singleNumberToChineseWord(lit) + "公里";
            }
        }
    }

    public static String formatDistToChStrForGuide(int nDist) {
        if (nDist <= 0) {
            return "未知";
        }
        if (nDist < 1000) {
            return numberToChineseWord(nDist) + "米";
        } else {
            nDist /= 1000;
            return numberToChineseWord(nDist) + "公里";
        }
    }

    /**
     * 将字符数组(以0结尾)转换成字符串
     *
     * @param array 字符数组
     * @return 字符串
     */
    public static String charArrayToString(char[] array) {
        if (array != null) {
            StringBuilder sb = new StringBuilder();
            for (char c : array) {
                if (c == 0) {
                    break;
                }
                sb.append(c);
            }
            return sb.toString();
        }
        return null;
    }

    /**
     * 将short数组(以0结尾)转换成字符串（通常包含中文字符）
     *
     * @param array short字符数组
     * @return 字符串
     */
    public static String shortArrayToString(short[] array) {
        if (array != null) {
            StringBuilder sb = new StringBuilder();
            for (short c : array) {
                if (c == 0) {
                    break;
                }
                sb.append((char) c);
            }
            return sb.toString();
        }
        return null;
    }

    /**
     * 转换储存空间信息
     *
     * @param nSize
     * @return
     */
    public static String byteToString(long nSize) {
        try {
            java.text.DecimalFormat df = new java.text.DecimalFormat();
            String strSize;
            if (nSize < 1024) {
                strSize = nSize + "B";
            } else if (nSize < 1048576) {
                // 1024*1024
                df.applyPattern("0");
                double d = (double) nSize / 1024;
                strSize = df.format(d) + "K";
            } else if (nSize < 1073741824) {
                // 1024*1024*1024
                df.applyPattern("0.0");
                double d = (double) nSize / 1048576;
                strSize = df.format(d) + "M";
            } else {
                // >1G
                df.applyPattern("0.0");
                double d = (double) nSize / 1073741824;
                strSize = df.format(d) + "G";
            }
            return strSize;
        } catch (Exception e) {
            return "0";
        }
    }

    public static String urlEncode(String str) {
        try {
            return URLEncoder.encode(str, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * 计算到达时间
     *
     * @param remainTime
     */
    public static String calculateArriveTimeStr(Context context, int remainTime) {
        long arriveTime = System.currentTimeMillis();
        Date curDate = new Date(arriveTime);

        arriveTime += (remainTime * 1000L);
        Date arriveDate = new Date(arriveTime);

        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
        String arriveTimeS = sdf.format(arriveDate);

        GregorianCalendar curCal = new GregorianCalendar();
        curCal.setTime(curDate);
        GregorianCalendar arriveCal = new GregorianCalendar();
        arriveCal.setTime(arriveDate);

        if (curCal.get(GregorianCalendar.DAY_OF_MONTH) == arriveCal
                .get(GregorianCalendar.DAY_OF_MONTH)) {
            if (0 == arriveCal.get(GregorianCalendar.HOUR_OF_DAY)) {
                arriveTimeS = String.format(
                        context.getResources().getString(R.string.rg_arrive_time_at_wee), arriveTimeS);
            } else {
                arriveTimeS = String.format(
                        context.getResources().getString(R.string.rg_arrive_time), arriveTimeS);
            }
        } else {
            int interval = getIntervalDays(curDate, arriveDate);
            if (interval == 1) {
                if (arriveCal.get(GregorianCalendar.HOUR_OF_DAY) >= 0 && arriveCal
                        .get(GregorianCalendar.HOUR_OF_DAY) < 4) {
                    arriveTimeS = String.format(context.getResources().getString(R.string.rg_arrive_time),
                            context.getResources().getString(R.string.rg_wee_hours));
                } else {
                    arriveTimeS = String.format(context.getResources().getString(R.string.rg_arrive_time),
                            context.getResources().getString(R.string.rg_tomorrow));
                }
            } else if (interval == 2) {
                arriveTimeS = String.format(context.getResources().getString(R.string.rg_arrive_time),
                        context.getResources().getString(R.string.rg_the_day_after_tomorrow));
            } else if (interval > 2) {
                arriveTimeS = String.format(
                        context.getResources().getString(R.string.rg_arrive_time_after_day), "" + interval);
            } else {
                arriveTimeS = String.format(
                        context.getResources().getString(R.string.rg_arrive_time), arriveTimeS);
            }
        }

        return arriveTimeS;
    }

    /**
     * 两个日期之间相隔的天数
     *
     * @param fDate
     * @param oDate
     * @return
     */
    private static int getIntervalDays(Date fDate, Date oDate) {
        if (null == fDate || null == oDate) {
            return 0;
        }

        long intervalMilli = oDate.getTime() - fDate.getTime();
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            fDate = sdf.parse(sdf.format(fDate));
            oDate = sdf.parse(sdf.format(oDate));
            intervalMilli = oDate.getTime() - fDate.getTime();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return (int) (intervalMilli / (24 * 60 * 60 * 1000));

    }

    /**
     * 计算到达时间
     *
     * @param remainTime 剩余时间
     */
    public static String calculateArriveTime(int remainTime) {
        long curTime = System.currentTimeMillis();
        Date curDate = new Date(curTime);
        long arriveTime = curTime + (remainTime * 1000);
        Date arriveDate = new Date(arriveTime);

        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
        String arriveTimeStr = sdf.format(arriveDate);

        GregorianCalendar curCal = new GregorianCalendar();
        curCal.setTime(curDate);
        GregorianCalendar arriveCal = new GregorianCalendar();
        arriveCal.setTime(arriveDate);

        if (curCal.get(GregorianCalendar.DAY_OF_MONTH) == arriveCal
                .get(GregorianCalendar.DAY_OF_MONTH)) {
            if (0 == arriveCal.get(GregorianCalendar.HOUR_OF_DAY)) {
                arriveTimeStr = String.format("凌晨%s", arriveTimeStr);
            }
        } else {
            int interval = getIntervalDays(curDate, arriveDate);
            if (interval == 1) {
                if (arriveCal.get(GregorianCalendar.HOUR_OF_DAY) >= 0 && arriveCal
                        .get(GregorianCalendar.HOUR_OF_DAY) < 4) {
                    arriveTimeStr = String.format("凌晨%s", arriveTimeStr);
                } else {
                    arriveTimeStr = String.format("明天%s", arriveTimeStr);
                }
            } else if (interval == 2) {
                arriveTimeStr = String.format("后天%s", arriveTimeStr);
            } else if (interval > 2) {
                arriveTimeStr = String.format("%s天后", "" + interval);
            }
        }
        return arriveTimeStr;
    }

    /**
     * 字符串超出长度省略
     *
     * @return
     */
    public static String strExcessLengthOmitted(String str, int length) {
        if (isEmpty(str)) {
            return "";
        }
        if (str.length() <= length) {
            return str;
        }
        return str.substring(0, length) + "...";
    }

    public static String getVerificationTime(String time) {
        if (StringUtils.isEmpty(time)) {
            return "";
        }
        if (time.contains("24:00")) {
            return time.replaceAll("24:00", "23:59");
        } else if (time.equals("24")) {
            return "24小时";
        }
        if (time.length() == 17) {
            time = time.substring(0, 5) + time.substring(8, 14);
        }
        return time;
    }

    public static String getDistanceUnit(String str) {
        if (str != null) {
            if (str.endsWith("km")) {
                return "公里";
            } else if (str.endsWith("m")) {
                return "米";
            }
        }
        return "";
    }

    public static String getEnDistanceUnit(String str) {
        if (str != null) {
            if (str.endsWith("km")) {
                return "km";
            } else if (str.endsWith("m")) {
                return "m";
            }
        }
        return "";
    }

    public static String getEnDistance(String str) {
        if (str != null) {
            if (str.endsWith("km")) {
                return str.substring(0, str.length() - 2);
            } else if (str.endsWith("m")) {
                return str.substring(0, str.length() - 1);
            }
        }
        return "";
    }

    /**
     * 获取两个字符串共同子字符串
     *
     * @param s1
     * @param s2
     * @return
     */
    public static String get2StrSubString(String s1, String s2) {
        if (TextUtils.isEmpty(s1) || TextUtils.isEmpty(s2)) {
            return "";
        }
        if (s2.contains(s1)) {
            return s1;
        }
        if (s1.contains(s2)) {
            return s1;
        }
        if (s1.length() > s2.length()) {
            String temp = s1;
            s1 = s2;
            s2 = temp;
        }
        int n = s1.length();
        int index = 0;
        ok:
        for (; n > 0; n--) {
            for (int i = 0; i < s1.length() - n + 1; i++) {
                String s = s1.substring(i, i + n);
                if (s2.indexOf(s) != -1) {
                    index = i;
                    break ok;
                }
            }
        }
        return s1.substring(index, index + n);
    }

    public static String getNearbyTitleStr(String title) {
        if (StringUtils.isEmpty(title)) {
            return "";
        }
        if (title.length() <= 12) {
            return title;
        }
        if (title.length() > 15 && title.contains("(") && title.contains(")") && title.endsWith(")")) {
            title = title.substring(0, title.indexOf("("));
        }
        if (title.length() <= 12) {
            return title;
        }
        if (title.contains("省") && title.contains("市")) {
            title = title.substring(title.indexOf("省") + 1);
            if (title.length() > 12) {
                title = title.substring(title.indexOf("市") + 1);
            }
        }
        if (title.length() <= 12) {
            return title;
        }
        return StringUtils.strExcessLengthOmitted(title, 12);
    }

    public static String arrayIntToString(ArrayList<Integer> integers) {
        if (integers == null || integers.isEmpty()) {
            return "";
        }
        String resInt = "";
        for (Integer integer : integers) {
            resInt = resInt + integer + " , ";
        }

        return resInt;
    }

    /**
     * 保留两位小数
     *
     * @param value
     * @return
     */
    public static String format2Decimal(double value) {
        return new Formatter().format("%.2f", value).toString();
    }

    public static boolean isSameString(String firstStr, String secondStr) {
        if (firstStr == null || secondStr == null) {
            return false;
        }
        return firstStr.equals(secondStr);
    }

    public static long getDistanceByStr(String str) {
        if (isEmpty(str)) {
            return 0;
        }
        if (str.contains("km") || str.contains("公里")) {
            return Math.round(Double.parseDouble(str.substring(0, str.length() - 2)) * 1000L);
        } else if (str.equals("m") || str.contains("米")) {
            return Integer.parseInt(str.substring(0, str.length() - 1));
        }
        return 0;
    }

    /**
     * 去除数字前字符
     */
    public static String getSplitString(String string) {
        int startIndex = 0;
        if (isNotEmpty(string)) {
            for (int i = 0; i < string.length(); i++) {
                if (isNumeric(string.substring(i, i + 1))) {
                    startIndex = i;
                    break;
                }
            }
            return string.substring(startIndex);
        } else {
            return "";
        }
    }

    public static String getPrice(String deepPrice, String price) {
        return !isEmpty(deepPrice) ? deepPrice : price;

    }

    public static boolean compare(String value, String value1) {
        if (TextUtils.isEmpty(value) && TextUtils.isEmpty(value1)) {
            return true;
        }
        if (TextUtils.isEmpty(value) || TextUtils.isEmpty(value1)) {
            return false;
        }
        return value.equals(value1);
    }

    /**
     * 将String类型的距离转为long类型
     *
     * @param distance
     * @return
     */
    public static long transferDistanceFromStrToLong(String distance) {
        if (isEmpty(distance)) {
            return 0;
        }
        distance = distance.trim();
        long result = 0L;
        try {
            if (distance.contains("km") || distance.contains("公里")) {
                result = Math.round(Double.parseDouble(distance.substring(0, distance.length() - 2)) * 1000L);
            } else if (distance.equals("m") || distance.contains("米")) {
                result = Integer.parseInt(distance.substring(0, distance.length() - 1));
            } else {
                result = Math.round(Double.parseDouble(distance));
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            return result;
        }
    }

    public static float stringToFloat(String value) {
        float desValue = 0f;
        try {
            desValue = Float.parseFloat(value);
        } catch (NumberFormatException e) {
            Logger.e(TAG, "stringToFloat exception:" + e.getMessage());
        }
        return desValue;
    }

    public static long stringToLong(String value) {
        long desValue = 0;
        try {
            desValue = Long.parseLong(value);
        } catch (NumberFormatException e) {
            Logger.e(TAG, "stringToLong exception:" + e.getMessage());
        }
        return desValue;
    }
}
