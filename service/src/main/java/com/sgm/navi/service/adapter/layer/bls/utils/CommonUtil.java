package com.sgm.navi.service.adapter.layer.bls.utils;

import android.content.Context;
import android.content.res.Resources;
import android.os.Build;
import android.os.Looper;
import android.text.TextUtils;

import com.autonavi.gbl.pos.model.GPSDatetime;
import com.sgm.navi.service.AppCache;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CommonUtil {

    private static final String TAG = "CommonUtil";

      /*
     * （1）首字母为大写字母
     * （2）车牌长度为6（油车）或7（新能源），并且车牌只包含大写字母和数字
     */
    private static String carnumRegex = "^[A-Z]+[A-Z0-9]{5,6}$";
    private static Pattern carNumberPattern = Pattern.compile(carnumRegex);

    /**
     * 获取当前线程信息, 线程名/id/是否主线程等
     */
    public static String getThreadInfo() {
        Thread thread = Thread.currentThread();
        return "name:" + thread.getName() + ",id:" + thread.getId() + ",isMain:" + isMainThread();
    }

    /**
     * 判断当前是否在主线程
     */
    public static boolean isMainThread() {
        return Looper.getMainLooper().getThread() == Thread.currentThread();
    }

    /**
     * 获取主线程id
     */
    public static long getMainThreadId() {
        return Looper.getMainLooper().getThread().getId();
    }

    /**
     * 获取当前线程id
     */
    public static long getcurrentThreadId() {
        return Thread.currentThread().getId();
    }

    public static String getIMEINew(Context context) {
        //we make this look like a valid IMEI
        String imei = "35" +
            Build.BOARD.length() % 10 +
            Build.BRAND.length() % 10 +
            Build.CPU_ABI.length() % 10 +
            Build.DEVICE.length() % 10 +
            Build.DISPLAY.length() % 10 +
            Build.HOST.length() % 10 +
            Build.ID.length() % 10 +
            Build.MANUFACTURER.length() % 10 +
            Build.MODEL.length() % 10 +
            Build.PRODUCT.length() % 10 +
            Build.TAGS.length() % 10 +
            Build.TYPE.length() % 10 +
            Build.USER.length() % 10; //13 digits
        return imei;
    }

    /**
     * 距离单位转换：米转公里
     *
     * @param dis
     * @return
     */
    public static String distanceUnitTransform(long dis) {
        StringBuffer sb = new StringBuffer();
        int distance = (int) dis;
        if (distance >= 1000) {
            int kiloMeter = distance / 1000;
            int leftMeter = distance % 1000;
            leftMeter = leftMeter / 100;
            if (kiloMeter > 100) {
                sb.append(kiloMeter);
                sb.append(getResources().getString(com.sgm.navi.service.R.string.km));
            } else if (leftMeter > 0) {
                sb.append(kiloMeter);
                sb.append(".");
                sb.append(leftMeter);
                sb.append(getResources().getString(com.sgm.navi.service.R.string.km));
            } else {
                sb.append(kiloMeter);
                sb.append(getResources().getString(com.sgm.navi.service.R.string.km));
            }
        } else {
            sb.append(distance);
            sb.append(getResources().getString(com.sgm.navi.service.R.string.meter));
        }
        return sb.toString();
    }

    public static Resources getResources() {
        return AppCache.getInstance().getMContext().getApplicationContext().getResources();
    }

    public static String formatTimeBySecond(int second) {
        String restTime = "";
        String minuteString = getResources().getString(com.sgm.navi.service.R.string.minute);
        String hourString = getResources().getString(com.sgm.navi.service.R.string.hour);

        int minute = (second + 30) / 60;

        if (minute < 60) { // 小于1小时
            if (minute == 0) {
                restTime = "<1" + minuteString;
            } else {
                restTime = minute + minuteString;
            }
        } else { // 大于小于1小时，小于24小时
            int hour = minute / 60;
            minute = minute % 60;
            if (minute > 0) {
                restTime = hour + hourString + minute + minuteString;
            } else {
                restTime = hour + hourString;
            }
        }

        return restTime;
    }

    public static boolean isCarNumber(String carnumber) {
         if (TextUtils.isEmpty(carnumber)) {
            return false;
        } else {
            Matcher m = carNumberPattern.matcher(carnumber);
            return m.matches();
        }
    }

    public static long parseGpsDateTime(GPSDatetime gpsDatetime) {
        if(gpsDatetime.year == 0){
            return  System.currentTimeMillis();
        }
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        try {
            Date date = simpleDateFormat.parse(gpsDatetime.year + "-" + gpsDatetime.month + "-" + gpsDatetime.day + " " + gpsDatetime.hour + ":" + gpsDatetime.minute + ":" + gpsDatetime.second);
            return date.getTime();
        } catch (ParseException e) {
            
            return System.currentTimeMillis();
        }
    }

    /**
     * 时间转换 日期
     *
     * @return
     */
    public static String switchDate(String date) {
        String switchTime = "";
        try {
            Calendar c = Calendar.getInstance();
            c.setTime(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(date));

            int year = c.get(Calendar.YEAR);
            int month = c.get(Calendar.MONTH) + 1;
            int day = c.get(Calendar.DAY_OF_MONTH);

            int hour = c.get(Calendar.HOUR_OF_DAY);
            int minute = c.get(Calendar.MINUTE);
            int second = c.get(Calendar.SECOND);
            int dayweek = c.get(Calendar.DAY_OF_WEEK);
            if (isNow(date)) {
                String hh = hour < 10 ? "0" + hour : hour + "";
                String mm = minute < 10 ? "0" + minute : minute + "";
                String ss = second < 10 ? "0" + second : second + "";
                switchTime = "今天 " + hh + ":" + mm + ":" + ss;
            } else {
                switchTime = month + "月" + day + "日";
            }
        } catch (ParseException e) {
            
        }
        return switchTime;
    }

    /**
     * 是否是今天
     *
     * @return
     */
    public static boolean isNow(String time) {
        try {
            SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
            Date date = formatter.parse(time);
            Date now = new Date();
            SimpleDateFormat sf = new SimpleDateFormat("yyyy-MM-dd");
            String nowDay = sf.format(now);
            String day = sf.format(date);
            return Objects.equals(day, nowDay);
        } catch (Exception e) {
            
        }
        return false;
    }

}
