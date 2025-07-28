package com.android.utils;

import android.content.Context;
import android.text.format.DateFormat;
import android.util.Log;

import com.android.utils.log.Logger;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

/**
 * @Introduce: 时间工具类.
 * @Author: lvww
 * @Date: 2023/11/13
 * @Description :System.currentTimeMillis()效率高于new Date().getTime(),yyyy代表年份 MM 代表月份 dd代表天数 DD代表当前天数在今天的第几天 EE代表周几
 */
public class TimeUtils {
    private static final SimpleDateFormat DATE_FORMAT_SHORT = new SimpleDateFormat("MM-dd HH:mm");
    private static final SimpleDateFormat DATE_FORMAT_FULL = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    /***线程不安全 但是效率高**/
    private final SimpleDateFormat simpleDateFormat;

    /***线程安全 但是效率不高**/
    private DateTimeFormatter dateTimeFormatter;

    public static final String FORMAT_Y_M_D = "yyyy-MM-dd";

    public static final String FORMAT_Y_M_D_E = "yyyy-MM-dd EE";

    public static final String FORMAT_Y_M_D_H_M = "yyyy-MM-dd HH:mm";

    public static final String FORMAT_Y_M_D_H_2 = "yyyyMMddHH";

    public static final String FORMAT_H_M = "HH:mm";

    public static final String FORMAT_Y_M_D_H_M_S = "yyyy-MM-dd HH:mm:ss";

    public static final String FORMAT_Y_M_D_E_H_M_S = "yyyy-MM-dd EE HH:mm:ss";

    public static final String FORMAT_YMD = "yyyy年MM月dd日";
    public static final String FORMAT_YMDHMS = "yyyy年MM月dd日 HH:mm:ss";

    public static final String FORMAT_MDHM = "MM月dd日 HH:mm";

    public static final String FORMAT_YYMMDD = "yyyyMMdd";

    private String dateFormat = FORMAT_Y_M_D_H_M_S;

    private String dateFormatHM = FORMAT_H_M;

    private static final long MILLSEC = 1000;

    private static final long SECOND = 1 * MILLSEC;

    private static final long MINUTES = 60 * SECOND;

    private static final long HOURS = 60 * MINUTES;

    private static final long DAYS = 24 * HOURS;

    private static final long MOTHS = 30 * DAYS;

    private static final long YEARS = 12 * MOTHS;

    private static final double EARTH_RADIUS = 6371000;

    private TimeUtils() {
        simpleDateFormat = new SimpleDateFormat();
        setDateFormat(FORMAT_Y_M_D_H_M_S);
//      dateTimeFormatter = new DateTimeFormatter();
    }

    /**
     * 设置格式.
     *
     * @param dateFormat
     */
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
        simpleDateFormat.applyPattern(dateFormat);
    }

    /**
     * 透出日期格式化类
     *
     * @return
     */
    public SimpleDateFormat getDateFormat() {
        return simpleDateFormat;
    }

    /**
     * 将一定格式的日期转化为毫秒.
     *
     * @param time
     * @return
     */
    public long convertTime2MillSecond(String time) {
        Date date = convertTime2Date(time);
        return date.getTime();
    }

    /**
     * 将一定格式的日期转化为秒数.
     *
     * @param time
     * @return
     */
    public long convertTime2Second(String time) {
        Date date = convertTime2Date(time);
        return convert2Second(date.getTime(), TimeUnit.MILLISECONDS);
    }

    /**
     * 将一定格式的字符串日期转化为Date.
     *
     * @param time
     * @return
     */
    public Date convertTime2Date(String time) {
        try {
            if (ConvertUtils.isEmpty(time))
                throw new RuntimeException("The format of the converted time is null");
            setDateFormat(dateFormat);
            return simpleDateFormat.parse(time);
        } catch (ParseException | RuntimeException e) {
            throw new RuntimeException("The format of the converted time is incorrect");
        }
    }

    /**
     * 将Date转换为固定格式的日期.
     *
     * @param time
     * @return
     */
    public String convertMill2Date(Date time) {
        if (null == time) throw new RuntimeException("The format of the converted time is null");
        setDateFormat(dateFormat);
        return simpleDateFormat.format(time);
    }

    /**
     * 将毫秒转换为固定格式的日期.
     *
     * @param time
     * @return
     */
    public String convertMill2Date(long time) {
        if (0 >= time) throw new RuntimeException("The format of the converted time is error");
        setDateFormat(dateFormat);
        Date date = new Date(time);
        return simpleDateFormat.format(date);
    }

    /**
     * 时间转换为天数.
     *
     * @param time 要转换的时间
     * @param unit 被转换的时间单位
     * @return
     */
    public long convert2Days(long time, TimeUnit unit) {
        return TimeUnit.DAYS.convert(time, unit);
    }

    /**
     * 时间转换为小时.
     *
     * @param time 要转换的时间
     * @param unit 被转换的时间单位
     * @return
     */
    public long convert2Hours(long time, TimeUnit unit) {
        return TimeUnit.HOURS.convert(time, unit);
    }

    /**
     * 时间转换为分钟.
     *
     * @param time 要转换的时间
     * @param unit 被转换的时间单位
     * @return
     */
    public long convert2Minutes(long time, TimeUnit unit) {
        return TimeUnit.MINUTES.convert(time, unit);
    }

    /**
     * 时间转换为秒.
     *
     * @param time 要转换的时间
     * @param unit 被转换的时间单位
     * @return
     */
    public long convert2Second(long time, TimeUnit unit) {
        return TimeUnit.SECONDS.convert(time, unit);
    }

    /**
     * 时间转换为毫秒.
     *
     * @param time 要转换的时间
     * @param unit 被转换的时间单位
     * @return
     */
    public long convert2MillSecond(long time, TimeUnit unit) {
        return TimeUnit.MILLISECONDS.convert(time, unit);
    }

    /**
     * 时间比较
     *
     * @param compareToDate 要比较的时间 单位·毫秒类型
     * @return true/false
     */
    public boolean equals(Date compareToDate) {
        return equals(getCurrentMillSec(), compareToDate);
    }

    /**
     * 时间比较,比较精确,可以精确到秒(取决于simpleDateFormat的时间格式).
     *
     * @param currentDate   被比较的时间
     * @param compareToDate 要比较的时间
     * @return true/false
     */
    public boolean equals(Date currentDate, Date compareToDate) {
        return ConvertUtils.equals(currentDate, compareToDate);
    }

    /**
     * 时间比较
     *
     * @param compareToDate 要比较的时间
     * @return true：getCurrentTime < compareToDate、false；getCurrentTime >= compareToDate
     */
    @Deprecated
    public boolean equals(String compareToDate) {
        return equals(getCurrentDate(), compareToDate);
    }

    /**
     * 时间比较
     *
     * @param currentDate   被比较的时间
     * @param compareToDate 要比较的时间
     * @return true：currentDate < compareToDate、false；currentDate >= compareToDate
     */
    public boolean equals(String currentDate, String compareToDate) {
        Date currentTime = convertTime2Date(currentDate);
        Date compareTime = convertTime2Date(compareToDate);
        return equals(currentTime, compareTime);
    }

    /**
     * 毫秒级时间比较.
     *
     * @param compareToDate 要比较的时间
     * @return
     */
    public boolean equals(long compareToDate) {
        return ConvertUtils.equals(getCurrentMillSeconds(), compareToDate);
    }

    /**
     * 毫秒级时间比较.
     *
     * @param currentDate   被比较的时间
     * @param compareToDate 要比较的时间
     * @return
     */
    public boolean equals(long currentDate, long compareToDate) {
        return ConvertUtils.equals(currentDate, compareToDate);
    }

    /**
     * 时间比较
     *
     * @param currentDate   被比较的时间
     * @param compareToDate 要比较的时间
     * @return true：currentDate > compareToDate、false；currentDate >= compareToDate
     */
    public boolean roughStringEquals(String currentDate, String compareToDate) {
        return ConvertUtils.equals(currentDate, compareToDate);
    }

    /**
     * 时间比较大小
     *
     * @param compareToDate 要比较的时间 单位·毫秒类型
     * @return true: arg > args
     */
    public boolean compareGreater(Date compareToDate) {
        return compareGreater(getCurrentMillSec(), compareToDate);
    }

    /**
     * 时间比较大小
     *
     * @param compareToDate 要比较的时间 单位·毫秒类型
     * @return true: arg > args
     */
    public boolean compareGreater(Date currentDate, Date compareToDate) {
        return 0 < currentDate.compareTo(compareToDate);
    }

    /**
     * 时间比较大小
     *
     * @param compareToDate 要比较的时间 单位·毫秒类型
     * @return true: arg > args
     */
    public boolean compareGreater(long compareToDate) {
        return ConvertUtils.compareGreater(getCurrentMillSeconds(), compareToDate);
    }

    /**
     * 时间比较大小
     *
     * @param currentDate   要比较的时间 单位·毫秒类型
     * @param compareToDate 要比较的时间 单位·毫秒类型
     * @return true: arg > args
     */
    public boolean compareGreater(long currentDate, long compareToDate) {
        return ConvertUtils.compareGreater(currentDate, compareToDate);
    }

    /**
     * 时间比大小 适合粗略的比较 两个时间参数格式最好一致，否则在同一年时无论月份时间是多少都返回true.
     *
     * @param currentDate   被比较的时间
     * @param compareToDate 要比较的时间
     * @return true：currentDate > compareToDate、false；currentDate >= compareToDate
     */
    public boolean roughStringGreater(String currentDate, String compareToDate) {
        return 0 < currentDate.compareTo(compareToDate);
    }

    /**
     * 计算时间差.
     *
     * @param current
     * @param date
     * @return 返回相差毫秒数
     */
    public long timeDiff(Date current, Date date) {
        return timeDiff(current.getTime(), date.getTime());
    }

    /**
     * 计算时间差.
     *
     * @param current
     * @param date
     * @return 返回相差毫秒数
     */
    public long timeDiff(String current, String date) {
        Date currentDate = convertTime2Date(current);
        Date dateTime = convertTime2Date(date);
        return timeDiff(currentDate.getTime(), dateTime.getTime());
    }

    /**
     * 计算时间差.
     *
     * @param current
     * @param date
     * @return 返回相差毫秒数
     */
    public long timeDiff(long current, long date) {
        return ConvertUtils.numberDiff(current, date);
    }

    /**
     * 计算时间差.
     *
     * @param current
     * @param date
     * @return 返回相差时间
     */
    public String countDate(Date current, Date date, String dateFormat) {
        setDateFormat(dateFormat);
        return countDate(timeDiff(current, date));
    }

    /**
     * 计算时间差.
     *
     * @param current
     * @param date
     * @return 返回相差时间
     */
    public String countDate(long current, long date, String dateFormat) {
        setDateFormat(dateFormat);
        return countDate(ConvertUtils.numberDiff(current, date));
    }

    /**
     * 计算时间差.
     *
     * @param current
     * @param date
     * @return 返回相差时间
     */
    public String countDate(String current, String date) {
        return countDate(timeDiff(current, date));
    }

    /**
     * 获取当前时间.
     *
     * @return 当前天数在当年的第几天 今年的第317天
     */
    public long getCurrentDayOfYear() {
        return Calendar.getInstance().get(Calendar.DAY_OF_YEAR);
    }

    /**
     * 获取当前时间.
     *
     * @return 当前天数在当月的第几天 本月的第13天
     */
    public long getCurrentDayOfMoth() {
        return Calendar.getInstance().get(Calendar.DAY_OF_MONTH);
    }

    /**
     * 获取当前时间.
     *
     * @return 当前天数在当周的第几天从周日开始 本周的第2天周一
     */
    public long getCurrentDayOfWeek() {
        return Calendar.getInstance().get(Calendar.DAY_OF_WEEK);
    }

    /**
     * 获取当前时间.
     *
     * @return 当月的天数 本月的第13天
     */
    public long getCurrentDay() {
        return Calendar.getInstance().get(Calendar.DATE);
    }

    /**
     * 获取当前时间.
     *
     * @return 距离格林时间19674天
     */
    public long getCurrentDays() {
        return TimeUnit.MILLISECONDS.toDays(getCurrentMillSeconds());
    }

    /**
     * 获取当前时间.
     *
     * @return 当天的第9点(可能时上午也可能时下午)
     */
    public long getCurrentHour() {
        return Calendar.getInstance().get(Calendar.HOUR);
    }

    /**
     * 获取当前时间.
     *
     * @return 距离格林时间472189小时
     */
    public long getCurrentHours() {
        return TimeUnit.MILLISECONDS.toHours(getCurrentMillSeconds());
    }

    /**
     * 获取当前时间.
     *
     * @return 当前小时的第18分
     */
    public long getCurrentMinute() {
        return Calendar.getInstance().get(Calendar.MINUTE);
    }

    /**
     * 获取当前时间.
     *
     * @return 距离格林时间28331358分
     */
    public long getCurrentMinutes() {
        return TimeUnit.MILLISECONDS.toMinutes(getCurrentMillSeconds());
    }

    /**
     * 获取当前时间.
     *
     * @return 当前分钟第56秒
     */
    public int getCurrentSecond() {
        return Calendar.getInstance().get(Calendar.SECOND);
    }

    /**
     * 获取当前时间.
     *
     * @return 距离格林时间1699881536秒
     */
    public long getCurrentSecondS() {
        return TimeUnit.MILLISECONDS.toSeconds(getCurrentMillSeconds());
    }

    /**
     * 获取当前时间(相比较下.
     * System.currentTimeMillis()效率最高.
     * new Date().getTime()次之.
     * Calendar.getInstance().getTimeInMillis()最慢.
     *
     * @return long类型距离格林时间1699881536892毫秒
     */
    public long getCurrentMillSeconds() {
        return System.currentTimeMillis();
    }

    /**
     * 获取当前时间.
     *
     * @return date Nov 13, 2023 9:18:41 PM
     */
    public Date getCurrentMillSec() {
        return Calendar.getInstance().getTime();
    }

    /**
     * 获取当前日期.
     *
     * @return yyyy-MM-dd
     */
    public String getCurrentDate() {
        setDateFormat(FORMAT_Y_M_D);
        return simpleDateFormat.format(new Date());
    }

    /**
     * 获取当前日期.
     *
     * @return yyyy-MM-dd EE
     */
    public String getCurrentDateEE() {
        setDateFormat(FORMAT_Y_M_D_E);
        return simpleDateFormat.format(new Date());
    }

    /**
     * 获取当前时间，精确到小时
     *
     * @return yyyyMMddHH
     */
    public String getCurrentTimeToHour() {
        setDateFormat(FORMAT_Y_M_D_H_2);
        return simpleDateFormat.format(new Date());
    }

    /**
     * 获取当前时间.
     *
     * @return yyyy-MM-dd HH:mm:ss
     */
    public String getCurrentTime() {
        setDateFormat(FORMAT_Y_M_D_H_M_S);
        return simpleDateFormat.format(new Date());
    }

    /**
     * 获取当前时间.
     *
     * @return yyyy-MM-dd EE HH:mm:ss
     */
    public String getCurrentTimeEE() {
        setDateFormat(FORMAT_Y_M_D_E_H_M_S);
        return simpleDateFormat.format(new Date());
    }

    /**
     * 在当前时间基础上增加或减少unit
     *
     * @param unit       增加减少的单位：天、月、年{@link Calendar}
     * @param addAndRoll 修改的数量
     * @return update after Calendar
     */
    public Calendar updateTime(int unit, int addAndRoll) {
        Calendar calendar = Calendar.getInstance();
        if (0 < addAndRoll) calendar.add(unit, addAndRoll);
        if (0 > addAndRoll) calendar.roll(unit, addAndRoll);
        return calendar;
    }

    /**
     * 在当前时间基础上增加或减少unit
     *
     * @param unit       增加减少的单位：天、月、年{@link Calendar}
     * @param addAndRoll 修改的数量
     * @return update after Calendar
     */
    public Calendar updateTime(Calendar calendar, int unit, int addAndRoll) {
        if (0 < addAndRoll) calendar.add(unit, addAndRoll);
        if (0 > addAndRoll) calendar.roll(unit, addAndRoll);
        return calendar;
    }

    /**
     * 在当前日期上增加一定时长后的日期,可以用来计算预计到达时间.
     *
     * @param millSec
     * @return
     */
    public String addDate(long millSec) {
        millSec += getCurrentMillSeconds();
        return convertMill2Date(millSec);
    }

    /**
     * 获取当前时间加上指定秒数后的时间，并以 HH:mm 格式返回
     *
     * @param seconds 要添加的秒数，例如 660 表示加 11 分钟
     * @return 格式为 "HH:mm" 的时间字符串
     */
    public String getCurrentTimePlusSeconds(long seconds) {
        long currentTimeMillis = getCurrentMillSeconds();
        long newTimeMillis = currentTimeMillis + (seconds) * 1000L;
        setDateFormat(FORMAT_H_M); // 设置格式为 HH:mm
        return simpleDateFormat.format(new Date(newTimeMillis));
    }

    public String getTimeStr(long second) {
        int minute = (int) ((second + 30) / 60);
        String restTime = "";
        if (minute < 60) {
            //小于1小时
            if (minute == 0) {
                minute = 1;
            }
            restTime = minute + "分钟";
        } else {
            //小于1天
            int hour = minute / 60;
            restTime = hour + "小时";
            minute = minute % 60;
            if (minute > 0) {
                restTime = restTime + minute + "分钟";
            }
        }
        return restTime;
    }

    public String getDistanceString(long dis) {
        StringBuffer sb = new StringBuffer();
        int distance = (int) dis;
        if (distance >= 1000) {
            int kiloMeter = distance / 1000;
            int leftMeter = distance % 1000;
            leftMeter = leftMeter / 100;
            if (kiloMeter > 100) {
                sb.append(kiloMeter);
                sb.append("公里");
            } else if (leftMeter > 0) {
                sb.append(kiloMeter);
                sb.append(".");
                sb.append(leftMeter);
                sb.append("公里");
            } else {
                sb.append(kiloMeter);
                sb.append("公里");
            }
        } else {
            sb.append(distance);
            sb.append("米");
        }
        return sb.toString();
    }

    /**
     * 距离单位转换
     * @param dis
     * @return
     */
    public String getDistanceMsg(long dis) {
        StringBuffer sb = new StringBuffer();
        int distance = (int) dis;
        if (distance >= 1000) {
            int kiloMeter = distance / 1000;
            int leftMeter = distance % 1000;
            leftMeter = leftMeter / 100;
            if (kiloMeter > 100) {
                sb.append(kiloMeter);
                sb.append("km");
            } else if (leftMeter > 0) {
                sb.append(kiloMeter);
                sb.append(".");
                sb.append(leftMeter);
                sb.append("km");
            } else {
                sb.append(kiloMeter);
                sb.append("km");
            }
        } else {
            sb.append(distance);
            sb.append("m");
        }
        return sb.toString();
    }

    private String countDate(long millSecond) {
        long year, moth, day, hour, minutes, second;
        StringBuffer stringBuffer = new StringBuffer("");
        if (YEARS <= millSecond) {
            year = millSecond / YEARS;
            millSecond -= YEARS * year;
            stringBuffer.append(year).append("年");
        }
        if (MOTHS <= millSecond) {
            moth = millSecond / MOTHS;
            millSecond -= MOTHS * moth;
            stringBuffer.append(moth).append("月");
        }
        if (DAYS <= millSecond) {
            day = millSecond / DAYS;
            millSecond -= DAYS * day;
            stringBuffer.append(day).append("天");
        }
        if (HOURS <= millSecond) {
            hour = millSecond / HOURS;
            millSecond -= HOURS * hour;
            stringBuffer.append(hour).append("小时");
        }
        if (MINUTES <= millSecond) {
            minutes = millSecond / MINUTES;
            millSecond -= MINUTES * minutes;
            stringBuffer.append(minutes).append("分钟");
        }
        if (MILLSEC <= millSecond) {
            second = millSecond / MILLSEC;
            millSecond -= MILLSEC * second;
            stringBuffer.append(second).append("秒");
        }
        return stringBuffer.toString();
    }

    /**
     * 卡片到达天数
     *
     * @param second
     * @return
     */
    public static int getScheduledDayNum(int second) {
        long timeLong = System.currentTimeMillis() + second * 1000;
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(timeLong);
        int dayDiff = getDayDiff(calendar);
        return dayDiff;
    }

    public static int getDayDiff(Calendar day) {
        /**
         * 今天日期
         */
        Calendar today = Calendar.getInstance();
        today.setTimeInMillis(System.currentTimeMillis());
        int dayDiff = day.get(Calendar.DAY_OF_YEAR);
        if (day.get(Calendar.YEAR) != today.get(Calendar.YEAR)) {
            dayDiff += getDaysInYear(today.get(Calendar.YEAR));
        }
        int todayDiff = today.get(Calendar.DAY_OF_YEAR);

        return dayDiff - todayDiff;
    }

    public static int getDaysInYear(int years) {
        Calendar cal = Calendar.getInstance();
        cal.set(years, Calendar.DECEMBER, 31);
        return cal.get(Calendar.DAY_OF_YEAR);
    }

    /**
     * 卡片到达时间
     */
    public static String getScheduledTime(Context context, int second) {
        StringBuffer timeBuffer = new StringBuffer();
        long timeLong = System.currentTimeMillis() + second * 1000;
        boolean isHoleDay = DateFormat.is24HourFormat(context);
        if (Logger.openLog) {
            Logger.i("getScheduledTime", "isHoleDay = " + isHoleDay);
        }
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(timeLong);
        int dayIndex = calendar.get(Calendar.AM_PM);
        int min = calendar.get(Calendar.MINUTE);
        int hour = calendar.get(Calendar.HOUR);
        String hourStr;
        String minStr;
        if (min < 10) {
            minStr = "0" + min;
        } else {
            minStr = String.valueOf(min);
        }
        if (isHoleDay && dayIndex == 1) {
            hour += 12;
        }
        if (isHoleDay && hour < 10) {
            hourStr = "0" + hour;
        } else {
            hourStr = String.valueOf(hour);
        }
        String time = hourStr + ":" + minStr;
        timeBuffer.append(time);
        return timeBuffer.toString();
    }


    /**
     * 将秒转换为N小时n分样式
     *
     * @param second
     * @return
     */
    public static String switchHourAndMimuteFromSecond(Context context, long second) {
        String restTime = "";
        String minuteString = context.getString(R.string.tbt_time_minute_unit);
        String hourString = context.getString(R.string.tbt_time_hour_unit);

        long minute = (second + 30) / 60;

        if (minute < 60) {
            // 小于1小时
            if (minute == 0) {
                restTime = "<1" + minuteString;
            } else {
                restTime = minute + minuteString;
            }
        } else {
            // 大于1小时，显示小时数
            long hour = minute / 60;
            minute = minute % 60;
            if (minute > 0) {
                restTime = hour + hourString + minute + minuteString;
            } else {
                restTime = hour + hourString;
            }
        }
        return restTime;
    }

    public static String getRemainInfo(Context context, int distance, int time) {
        StringBuilder builder = new StringBuilder();
        String[] etaDistance = ConvertUtils.formatDistanceArray(context, distance);
        builder.append(etaDistance[0]).append(etaDistance[1]).append(" ")
                .append(switchHourAndMimuteFromSecond(context, time));
        return builder.toString();
    }

    public static String getRemainingTime(Context context, int time) {
        return switchHourAndMimuteFromSecond(context, time);
    }

    public static String getRemainingMileage(Context context, int distance) {
        StringBuilder builder = new StringBuilder();
        String[] etaDistance = ConvertUtils.formatDistanceArray(context, distance);
        builder.append(etaDistance[0]).append(etaDistance[1]);
        return builder.toString();
    }

    public static String getArriveDay(int time) {
        String arriveDay = "";
        // 到达天数
        if (getScheduledDayNum(time) == 0) {
            arriveDay = "";
        } else {
            arriveDay = "+" + getScheduledDayNum(time);
        }
        return arriveDay;
    }

    public static String getArriveTime(Context context, int time) {
        String arriveTime = "";
        String textArriveEta = getScheduledTime(context, time);
        // 到达时间
        int arriveDay = getScheduledDayNum(time);
        if (arriveDay == 0) {
            // 当天到达
            arriveTime = textArriveEta;
        } else if (arriveDay == 1) {
            // 第二天到达
            arriveTime = textArriveEta;
        } else {
            // 第三天及以后到达
            arriveTime = textArriveEta;
        }
        return arriveTime;
    }

    /**
     * 时间转换 几分钟前，几小时前
     *
     * @param time
     * @return
     */
    public static String switchTime(long time) {
        try {
            long currentTime = System.currentTimeMillis();
            long targetTime = time * 1000L; // 转换为毫秒
            long differenceValue = Math.abs(currentTime - targetTime);

            if (differenceValue < 60_000L) { // 1分钟内
                return "刚刚";
            } else if (differenceValue < 3_600_000L) { // 1小时内
                return (differenceValue / 60_000L) + "分钟前";
            } else if (differenceValue < 86_400_000L) { // 24小时内
                return (differenceValue / 3_600_000L) + "小时前";
            } else {
                return DATE_FORMAT_SHORT.format(targetTime);
            }
        } catch (Exception e) {
            // 可以根据需要记录日志
            return "未知时间";
        }
    }

    /**
     * 秒 转换成 HH:MM:SS 格式
     * @param seconds
     * @return
     */
    public static String formatSecondsToHHMMSS(int seconds) {
        int hours = seconds / 3600;
        int minutes = (seconds % 3600) / 60;
        int secs = seconds % 60;

        // 使用 String.format 来确保小时、分钟和秒都是两位数
        return String.format("%02d:%02d:%02d", hours, minutes, secs);
    }

    /**
     * yyyy-MM-dd EE HH:mm:ss 转换成 MM月dd日 HH:mm
     * @param originalDateString
     * @return
     */
    public static String convertDateFormat(String originalDateString) {
        // 定义原始日期时间格式
        SimpleDateFormat originalDateFormat = new SimpleDateFormat(FORMAT_Y_M_D_H_M_S, Locale.CHINA);
        // 定义目标日期时间格式
        SimpleDateFormat targetDateFormat = new SimpleDateFormat(FORMAT_MDHM, Locale.CHINA);

        try {
            // 解析原始日期时间字符串为Date对象
            Date date = originalDateFormat.parse(originalDateString);
            // 将Date对象格式化为目标字符串
            return targetDateFormat.format(date);
        } catch (ParseException e) {
            e.printStackTrace();
            return "日期时间格式错误";
        }
    }

    /**
     * 限行日期转换
     */
    public static String convertYMD(){
        try {
            final Date currentDate = new Date();
            final SimpleDateFormat dateFormat = new SimpleDateFormat(FORMAT_YYMMDD,Locale.CHINA);
            return dateFormat.format(currentDate);
        }catch (IllegalArgumentException e){
            e.printStackTrace();
        }
        return "";
    }

    public static boolean isTimeDifferenceGreaterThanOneWeek(long time1, long time2) {
        Calendar calendar1 = Calendar.getInstance();
        calendar1.setTimeInMillis(time1);

        Calendar calendar2 = Calendar.getInstance();
        calendar2.setTimeInMillis(time2);

        long diffTime = calendar2.getTimeInMillis() - calendar1.getTimeInMillis();
        if (diffTime < 0) {
            return false;
        }
        long differenceInDays = diffTime / (24 * 60 * 60 * 1000);

        return differenceInDays > 7;
    }

    public static boolean isCurrentTimeInRange(String timeRange) {
        try {
            // 分割时间范围字符串
            String[] times = timeRange.split("~");
            if (times.length != 2) {
                return false;
            }
            // 创建时间格式化器
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");
            // 解析开始和结束时间
            LocalTime startTime = LocalTime.parse(times[0], formatter);
            LocalTime endTime = LocalTime.parse(times[1], formatter);
            // 获取当前时间
            LocalTime currentTime = LocalTime.now().truncatedTo(java.time.temporal.ChronoUnit.MINUTES);
            // 判断当前时间是否在范围内
            if (endTime.isAfter(startTime)) {
                // 正常时间范围（不跨午夜）
                return !currentTime.isBefore(startTime) && !currentTime.isAfter(endTime);
            } else {
                // 跨午夜的时间范围（如22:00~02:00）
                return !currentTime.isBefore(startTime) || !currentTime.isAfter(endTime);
            }
        } catch (DateTimeParseException e) {
            e.printStackTrace();
            return false;
        }
    }

    public static String formatTimeRange(String timeRange) {
        String[] parts = timeRange.split("~");
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < parts.length; i++) {
            if (i > 0) result.append("~");
            result.append(parts[i].substring(0, 5));
        }
        Logger.d("huangli","result: "+result.toString());
        return result.toString();
    }

    /**
     * 是否是上班时间和下班时间
     */
    public static boolean isCurrentTimeInSpecialRange(boolean workHours) {
        if(workHours){
            return isCurrentTimeInRange("06:00~10:00");
        }else {
            return isCurrentTimeInRange("17:00~24:00");
        }
    }


    public static TimeUtils getInstance() {
        return Helper.timeU;
    }

    private static class Helper {
        private static final TimeUtils timeU = new TimeUtils();
    }

}
