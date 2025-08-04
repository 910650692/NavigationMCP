package com.android.utils.log;

import android.util.Log;

import com.android.utils.ConvertUtils;
import com.android.utils.SpUtils;
import com.google.gson.ExclusionStrategy;
import com.google.gson.FieldAttributes;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * @Introduce: 实体.
 * @Author: lvww
 * @Date: 2023/10/11
 * @Description:
 */
public class LogUtils {
    private static final int VERBOSE = Log.VERBOSE;

    private static final int DEBUG = Log.DEBUG;

    private static final int INFO = Log.INFO;

    private static final int WARN = Log.WARN;

    private static final int ERROR = Log.ERROR;
    /*** 默认Tag值 **/
    private static String DEFAULT_TAG = "";

    /*** log默认输出级别 **/
    private static int LOG_LEVEL = Log.DEBUG;

    private static final int LOG_MAX_LENGTH = 1024 * 2;

    private static final Gson gson = new GsonBuilder()
            .disableHtmlEscaping()
            .addSerializationExclusionStrategy(new ExclusionStrategy() {
                @Override
                public boolean shouldSkipField(FieldAttributes f) {
                    return !isBasicType(f.getDeclaredClass());
                }

                @Override
                public boolean shouldSkipClass(Class<?> clazz) {
                    return false;
                }

                private boolean isBasicType(Class<?> clazz) {
                    return clazz.isPrimitive() || clazz == String.class || clazz == Integer.class ||
                            clazz == Long.class || clazz == Boolean.class || clazz == Float.class ||
                            clazz == Double.class;
                }
            })
            .create();

    public static void setDefaultTag(String defaultTag) {
        DEFAULT_TAG = defaultTag;
    }

    /**
     * Set log print level/
     *
     * @param level log level
     */
    protected static void setLogLevel(int level) {
        LOG_LEVEL = level;
    }

    protected static boolean isDebugLevel() {
        return !(DEBUG < LOG_LEVEL);
    }

    /**
     * Log switch.
     *
     * @param logSwitch log switch
     */
    protected static void switchLog(boolean logSwitch) {
        SpUtils.getInstance().putBoolean(SpUtils.SP_KEY_LOG_SWITCH, logSwitch);
        /*** log开关：true 开、false 关 **/
        LOG_LEVEL = logSwitch ? Log.DEBUG : Log.ERROR;
    }

    /**
     * @param logSwitch
     * @param level
     */
    protected static void initLogUtils(boolean logSwitch, int level, String defaultTag) {
        switchLog(logSwitch);
        setLogLevel(level);
        setDefaultTag(defaultTag);
    }

    protected static void verbose(Object args) {
        if (VERBOSE < LOG_LEVEL) return;
        verbose(DEFAULT_TAG, args);
    }

    protected static void verbose(String tag, Object... args) {
        if (VERBOSE < LOG_LEVEL) return;
        assembleLog(VERBOSE, tag, args);
    }

    protected static void debug(Object args) {
        if (DEBUG < LOG_LEVEL) return;
        debug(DEFAULT_TAG, args);
    }

    protected static void debug(String tag, Object... args) {
        if (DEBUG < LOG_LEVEL) return;
        assembleLog(DEBUG, tag, args);
    }


    protected static void info(Object args) {
        if (INFO < LOG_LEVEL) return;
        info(DEFAULT_TAG, args);
    }

    protected static void info(String tag, Object... args) {
        if (INFO < LOG_LEVEL) return;
        assembleLog(INFO, tag, args);
    }

    protected static void warn(Object args) {
        if (WARN < LOG_LEVEL) return;
        warn(DEFAULT_TAG, args);
    }

    protected static void warn(String tag, Object... args) {
        if (WARN < LOG_LEVEL) return;
        assembleLog(WARN, tag, args);
    }

    protected static void error(Object args) {
        if (ERROR < LOG_LEVEL) return;
        error(DEFAULT_TAG, args);
    }

    protected static void error(String tag, Object... args) {
        if (ERROR < LOG_LEVEL) return;
        assembleLog(ERROR, tag, args);
    }

    private static void printLog(int level, String tag, String msg) {
        StringBuffer stringBuffer = getMethodName();
        switch (level) {
            case VERBOSE:
                Log.v(tag, stringBuffer.append(msg).toString());
                break;
            case DEBUG:
                Log.d(tag, stringBuffer.append(msg).toString());
                break;
            case INFO:
                Log.i(tag, stringBuffer.append(msg).toString());
                break;
            case WARN:
                Log.w(tag, stringBuffer.append(msg).toString());
                break;
            case ERROR:
                Log.e(tag, stringBuffer.append(msg).toString());
                break;
            default:
                break;
        }
    }

    private static void assembleLog(int level, String tag, Object... args) {
        if (ConvertUtils.isEmpty(tag)) tag = getDefaultTag();
        recursionLog(level, tag, convertJson(args));
    }

    private static void recursionLog(int level, String tag, String json) {
        if (ConvertUtils.isEmpty(json)) return;
        if (json.length() > 5 * LOG_MAX_LENGTH) return;
        if (LOG_MAX_LENGTH > json.length()) printLog(level, tag, json);
        else {
            String msg = json.substring(0, LOG_MAX_LENGTH);
            printLog(level, tag, msg);
            String newJson = json.replace(msg, "");
            recursionLog(level, tag, newJson);
        }
    }

    protected static String getCurrentTagName() {
        return getCurrentTag();
    }

    protected static String getDefaultTagName() {
        if (ConvertUtils.isEmpty(DEFAULT_TAG)) return getDefaultTag();
        return DEFAULT_TAG;
    }

    private static String convertJson(Object... args) {
        try {
            StringBuffer stringBuffer = new StringBuffer();
            if (ConvertUtils.isEmpty(args)) stringBuffer.append("Log incorrect output format");
            else {
                int length = args.length;
                for (int i = 0; i < length; i++) {
                    Object obj = args[i];
                    if (ConvertUtils.isEmpty(obj)) {
                        stringBuffer.append("null");
                        if (i < length - 1) stringBuffer.append(",");
                        continue;
                    }
                    if (obj instanceof String) stringBuffer.append(obj);
                    else stringBuffer.append(gson.toJson(obj));
                    if (i < length - 1) stringBuffer.append(",");
                }
            }
            return stringBuffer.toString();
        } catch (Exception e) {
            return e.toString();
        }
    }

    private static StringBuffer getMethodName() {
        StringBuffer stringBuilder = new StringBuffer();
        StackTraceElement[] traceElements = Thread.currentThread().getStackTrace();
        int stackOffset = getStackOffset(traceElements);
        stringBuilder.append("(").append(traceElements[stackOffset].getFileName())
                .append(":").append(traceElements[stackOffset].getLineNumber())
                .append(") ").append(traceElements[stackOffset].getMethodName()).append("() -> ");
        return stringBuilder;
    }

    private static String getCurrentTag() {
        StackTraceElement[] traceElements = Thread.currentThread().getStackTrace();
        int stackOffset = getStackOffset(traceElements);
        stackOffset += 1;
        return traceElements[stackOffset].getFileName();
    }

    private static String getDefaultTag() {
        StackTraceElement[] traceElements = Thread.currentThread().getStackTrace();
        int stackOffset = getStackOffset(traceElements);
        return traceElements[stackOffset].getFileName();
    }

    private static int getStackOffset(StackTraceElement... traceElements) {
        for (int i = 3; i < traceElements.length; i++) {
            if (traceElements[i] == null) {
                continue;
            }
            String name = traceElements[i].getClassName();
            if (ConvertUtils.equalsIgnoreCase(LogUtils.class.getName(), name)
                    && ConvertUtils.equalsIgnoreCase(Logger.class.getName(), name)) {
                return i;
            }
        }
        return -1;
    }

    protected static void printStackTrace(String tag, boolean info) {
        StackTraceElement[] traceElements = Thread.currentThread().getStackTrace();
        StringBuilder stackTrace = new StringBuilder();
        for (StackTraceElement element : traceElements) {
            if (element != null) {
                String elementStr = element.toString();
                if (elementStr.contains("com.sgm")) {
                    stackTrace.append(elementStr).append("\n");
                }
            }
        }
        if (info) {
            info(tag, stackTrace.toString());
        } else {
            debug(tag, stackTrace.toString());
        }
    }
}