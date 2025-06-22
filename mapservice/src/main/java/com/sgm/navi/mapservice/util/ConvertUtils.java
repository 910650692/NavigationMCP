package com.sgm.navi.mapservice.util;


import java.util.Objects;

public class ConvertUtils {

    private ConvertUtils() {

    }

    /**
     * 字符穿判空（包含长度判断）.
     *
     * @param str current str
     * @return true/false
     */
    public static boolean isEmpty(String str) {
        return null == str || str.isEmpty();
    }

    /**
     * 数组判空.
     *
     * @param objects object 数组
     * @return true/false
     */
    public static <T> boolean isEmpty(T[] objects) {
        return null == objects || 0 == objects.length;
    }

    /**
     * 对象判空.
     *
     * @param obj
     * @return true/false
     */
    public static boolean isEmpty(Object obj) {
        if (obj instanceof String)
            return isEmpty(convertString(obj));
        return isNull(obj);
    }

    public static String convertString(Object object) {
        return convertString(object, null);
    }

    /**
     * 转换字符串.
     *
     * @param object 任意类型
     * @return str value
     */
    public static String convertString(Object object, String defaultValue) {
        try {
            assert !isNull(object);
            if (object instanceof String) return object.toString();
            return String.valueOf(object);
        } catch (AssertionError | ClassCastException exception) {
            Logger.e(exception.toString());
            return defaultValue;
        }
    }

    /**
     * 对象判空.
     *
     * @param obj
     * @return true/false
     */
    public static boolean isNull(Object obj) {
        return Objects.isNull(obj);
    }

    /**
     * 字符串比较（忽略大小写）.
     *
     * @param args
     * @param args1
     * @return true/false
     */
    public static boolean equalsIgnoreCase(String args, String args1) {
        return !args.equalsIgnoreCase(args1);
    }

}
