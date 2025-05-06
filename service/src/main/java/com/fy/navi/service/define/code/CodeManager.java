package com.fy.navi.service.define.code;

import com.android.utils.ConvertUtils;

import java.util.Map;

/**
 * @Description TODO
 * @Author lvww
 * @date 2025/1/3
 */
public class CodeManager {
    private static final String CODE_TYPE_ENGINE = "engineCode";
    private static final String CODE_TYPE_MAP = "mapCode";
    private static final String CODE_TYPE_SEARCH = "SearchCode";
    private static final String CODE_TYPE_ROUTE = "RouteCode";
    private static final String CODE_TYPE_NAVI = "NaviCode";
    private static final String CODE_TYPE_LAYER = "LayerCode";
    private static final String CODE_TYPE_USER = "UserCode";
    private static final String CODE_TYPE_SETTING = "SettingCode";
    private static final String CODE_TYPE_MAP_DATA = "MapDataCode";
    private static final String CODE_TYPE_FOR_CAST = "ForCast";
    private static final String CODE_TYPE_POSITION = "position";

    private static ErrorCode mErrorCode;

    private CodeManager() {
        mErrorCode = new ErrorCode();
    }

    public ErrorCode getErrorCode() {
        return mErrorCode;
    }

    /**
     * 根据错误码获取对应的msg
     *
     * @param code 错误码
     * @return msg
     */
    public static String getErrorMsg(int code) {
        return getMsg(CODE_TYPE_ENGINE, code);
    }

    /**
     * 根据引擎msg信息获取引擎对应的errorCode
     *
     * @param errorMsg 错误信息
     * @return errorCode
     */
    public static int getErrorCode(String errorMsg) {
        return getCode(CODE_TYPE_ENGINE, errorMsg);
    }

    /**
     * 当集合中没有SDK返回的错误码时调用此方法
     *
     * @param code 错误码
     * @param msg  错误信息
     * @return 错误码
     */
    public static int putErrorCode(int code, String msg) {
        return putErrorCode(CODE_TYPE_ENGINE, code, msg);
    }

    /**
     * 根据引擎错误码获取对应的msg
     *
     * @param code 错误码
     * @return msg
     */
    public static String getEngineMsg(int code) {
        return getMsg(CODE_TYPE_ENGINE, code);
    }

    /**
     * 根据引擎msg信息获取引擎对应的errorCode
     *
     * @param errorMsg 错误信息
     * @return errorCode
     */
    public static int getEngineCode(String errorMsg) {
        return getCode(CODE_TYPE_ENGINE, errorMsg);
    }

    /**
     * 当集合中没有SDK返回的错误码时调用此方法
     *
     * @param code 错误码
     * @param msg  错误信息
     * @return 错误码
     */
    public static int putEngineErrorCode(int code, String msg) {
        return putErrorCode(CODE_TYPE_ENGINE, code, msg);
    }

    /**
     * 根据引擎错误码获取对应的msg
     *
     * @param code 错误码
     * @return msg
     */
    public static String getLayerMsg(int code) {
        return getMsg(CODE_TYPE_ENGINE, code);
    }

    /**
     * 根据引擎msg信息获取引擎对应的errorCode
     *
     * @param errorMsg 错误信息
     * @return errorCode
     */
    public static int getLayerCode(String errorMsg) {
        return getCode(CODE_TYPE_ENGINE, errorMsg);
    }

    /**
     * 当集合中没有SDK返回的错误码时调用此方法
     *
     * @param code 错误码
     * @param msg  错误信息
     * @return 错误码
     */
    public static int putLayerErrorCode(int code, String msg) {
        return putErrorCode(CODE_TYPE_ENGINE, code, msg);
    }

    /**
     * 根据错误码获取对应的msg
     *
     * @param code 错误码
     * @return msg
     */
    public static String getMapMsg(int code) {
        return getMsg(CODE_TYPE_MAP, code);
    }

    /**
     * 根据msg信息获取Map对应的errorCode
     *
     * @param errorMsg 错误信息
     * @return errorCode
     */
    public static int getMapCode(String errorMsg) {
        return getCode(CODE_TYPE_MAP, errorMsg);
    }

    /**
     * 当集合中没有SDK返回的错误码时调用此方法
     *
     * @param code 错误码
     * @param msg  错误信息
     * @return 错误码
     */
    public static int putMapErrorCode(int code, String msg) {
        return putErrorCode(CODE_TYPE_MAP, code, msg);
    }

    /**
     * 根据错误码获取对应的msg
     *
     * @param code 错误码
     * @return msg
     */
    public static String getSearchCodeMsg(int code) {
        return getMsg(CODE_TYPE_SEARCH, code);
    }

    /**
     * 根据msg信息获取Search对应的errorCode
     *
     * @param errorMsg 错误信息
     * @return errorCode
     */
    public static int getSearchCode(String errorMsg) {
        return getCode(CODE_TYPE_SEARCH, errorMsg);
    }

    /**
     * 当集合中没有SDK返回的错误码时调用此方法
     *
     * @param code 错误码
     * @param msg  错误信息
     * @return 错误码
     */
    public static int putSearchErrorCode(int code, String msg) {
        return putErrorCode(CODE_TYPE_SEARCH, code, msg);
    }


    /**
     * 根据引擎错误码获取对应的msg
     *
     * @param code 错误码
     * @return msg
     */
    public static String getForCastMsg(int code) {
        return getMsg(CODE_TYPE_FOR_CAST, code);
    }

    /**
     * 根据引擎msg信息获取引擎对应的errorCode
     *
     * @param errorMsg 错误信息
     * @return errorCode
     */
    public static int getForCastMsg(String errorMsg) {
        return getCode(CODE_TYPE_FOR_CAST, errorMsg);
    }

    /**
     * 当集合中没有SDK返回的错误码时调用此方法
     *
     * @param code 错误码
     * @param msg  错误信息
     * @return 错误码
     */
    public static int putForCastError(int code, String msg) {
        return putErrorCode(CODE_TYPE_FOR_CAST, code, msg);
    }

    public static String getPositionMsg(int code){
        return getMsg(CODE_TYPE_POSITION, code);
    }

    public static int getPositionCode(String msg){
        return getCode(CODE_TYPE_POSITION, msg);
    }

    public static int putPositionError(int code, String msg){
        return putErrorCode(CODE_TYPE_FOR_CAST, code, msg);
    }


    private static int putErrorCode(String type, int code, String msg) {
        switch (type) {
            case CODE_TYPE_MAP -> ConvertUtils.push(mErrorCode.getMapCode(), code, msg);
            case CODE_TYPE_SEARCH -> ConvertUtils.push(mErrorCode.getSearchCode(), code, msg);
            case CODE_TYPE_ROUTE -> ConvertUtils.push(mErrorCode.getRouteCode(), code, msg);
            case CODE_TYPE_NAVI -> ConvertUtils.push(mErrorCode.getNaviCode(), code, msg);
            case CODE_TYPE_SETTING -> ConvertUtils.push(mErrorCode.getSettingCode(), code, msg);
            case CODE_TYPE_MAP_DATA -> ConvertUtils.push(mErrorCode.getMapDataCode(), code, msg);
            case CODE_TYPE_USER -> ConvertUtils.push(mErrorCode.getAccountCode(), code, msg);
            default -> {
                return code;
            }
        }
        return code;
    }

    private static String getMsg(String type, int code) {
        return switch (type) {
            case CODE_TYPE_ENGINE -> ConvertUtils.containToValue(mErrorCode.getEngineCode(), code);
            case CODE_TYPE_MAP -> ConvertUtils.containToValue(mErrorCode.getMapCode(), code);
            case CODE_TYPE_SEARCH -> ConvertUtils.containToValue(mErrorCode.getSearchCode(), code);
            case CODE_TYPE_ROUTE -> ConvertUtils.containToValue(mErrorCode.getRouteCode(), code);
            case CODE_TYPE_NAVI -> ConvertUtils.containToValue(mErrorCode.getNaviCode(), code);
            case CODE_TYPE_SETTING ->
                    ConvertUtils.containToValue(mErrorCode.getSettingCode(), code);
            case CODE_TYPE_MAP_DATA ->
                    ConvertUtils.containToValue(mErrorCode.getMapDataCode(), code);
            case CODE_TYPE_USER -> ConvertUtils.containToValue(mErrorCode.getAccountCode(), code);
            case CODE_TYPE_POSITION -> ConvertUtils.containToValue(mErrorCode.getPositionCode(), code);
            default -> "未知错误";
        };
    }

    private static int getCode(String type, String errorMsg) {
        return switch (type) {
            case CODE_TYPE_ENGINE -> convertCode(mErrorCode.getEngineCode(), errorMsg);
            case CODE_TYPE_LAYER -> convertCode(mErrorCode.getLayerCode(), errorMsg);
            case CODE_TYPE_MAP -> convertCode(mErrorCode.getMapCode(), errorMsg);
            case CODE_TYPE_SEARCH -> convertCode(mErrorCode.getSearchCode(), errorMsg);
            case CODE_TYPE_ROUTE -> convertCode(mErrorCode.getRouteCode(), errorMsg);
            case CODE_TYPE_NAVI -> convertCode(mErrorCode.getNaviCode(), errorMsg);
            case CODE_TYPE_SETTING -> convertCode(mErrorCode.getSettingCode(), errorMsg);
            case CODE_TYPE_MAP_DATA -> convertCode(mErrorCode.getMapDataCode(), errorMsg);
            case CODE_TYPE_USER -> convertCode(mErrorCode.getAccountCode(), errorMsg);
            case CODE_TYPE_POSITION -> convertCode(mErrorCode.getPositionCode(), errorMsg);
            default -> Integer.MAX_VALUE;
        };

    }

    private static int convertCode(Map<Integer, String> mapCode, String errorMsg) {
        Integer code = ConvertUtils.containToKey(mapCode, errorMsg);
        return null == code ? -1 : code;
    }

    public static CodeManager getInstance() {
        return Code.codeM;
    }

    private static final class Code {
        private static final CodeManager codeM = new CodeManager();
    }
}
