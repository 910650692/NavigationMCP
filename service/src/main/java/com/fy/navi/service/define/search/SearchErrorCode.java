package com.fy.navi.service.define.search;

import androidx.annotation.IntDef;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索错误码定义接口，包含错误码的枚举值及其定义，用于约束使用范围
 * @CreateDate: $ $
 */
public final class SearchErrorCode {
    private SearchErrorCode() {

    }

    // 通用错误码
    public static final int ERROR_UNKNOWN = Integer.MIN_VALUE; // 未知错误
    public static final int ERROR_FAILED = -1; // 操作失败
    public static final int ERROR_OK = 0; // 操作成功
    public static final int ERROR_DOING = 1; // 操作进行中
    public static final int ERROR_NOT_INITIALIZED = 2; // 未初始化

    // 参数相关错误
    public static final int ERROR_INVALID_PARAM = 3; // 无效参数
    public static final int ERROR_NULL_PARAM = 4; // 参数为 null

    // 对象相关错误
    public static final int ERROR_NULL_OBJECT = 5; // 对象为空

    // 文件和路径相关错误
    public static final int ERROR_DB_NOT_READY = 6; // 数据库未就绪
    public static final int ERROR_INVALID_FILE_NAME = 7; // 文件名称错误
    public static final int ERROR_FILE_NOT_EXIST = 8; // 文件不存在
    public static final int ERROR_FILE_UPDATING = 9; // 文件更新中
    public static final int ERROR_INVALID_PATH_NAME = 10; // 路径名称错误
    public static final int ERROR_PATH_NOT_EXIST = 11; // 路径不存在

    // 配置相关错误
    public static final int ERROR_SET_OFFLINE_MODE = 12; // 设置离线模式失败
    public static final int ERROR_GUIDE_INIT_FAILED = 13; // 引导初始化失败
    public static final int ERROR_INVALID_CONFIG_PATH = 14; // 配置文件路径无效
    public static final int ERROR_INVALID_CONFIG_FILE = 15; // 配置文件无效

    // 用户和权限相关错误
    public static final int ERROR_INVALID_USER = 16; // 未授权用户
    public static final int ERROR_AUTHENTICATION_FAILED = 26; // 鉴权失败

    // 数据相关错误
    public static final int ERROR_INVALID_LON_LAT = 21; // 无效经纬度
    public static final int ERROR_INVALID_RECT = 22; // 非法矩形
    public static final int ERROR_INVALID_POINT = 23; // 无效点坐标
    public static final int ERROR_ARRAY_EMPTY = 24; // 数组为空
    public static final int ERROR_ARRAY_INDEX_OUT_OF_BOUNDS = 25; // 数组索引越界

    // 网络相关错误
    public static final int ERROR_NET_CANCEL = 33554432; // 操作取消
    public static final int ERROR_NET_FAILED = 33554433; // 网络错误
    public static final int ERROR_NET_UNREACHABLE = 33554434; // 无网络
    public static final int ERROR_NET_INVALID_PARAM = 33554435; // 网络参数无效
    public static final int ERROR_NET_RESPONSE_NULL = 33554436; // 网络响应为空
    public static final int ERROR_NET_RESPONSE_ERROR = 33554437; // 网络响应错误

    // 压缩相关错误
    public static final int ERROR_ZIP = 33619968; // 压缩错误
    public static final int ERROR_UNZIP = 33619969; // 解压错误

    // 离线和在线相关错误
    public static final int ERROR_OFFLINE_NO_DATA = 1090584577; // 本地没有离线数据
    public static final int ERROR_ONLINE_TO_OFFLINE_NO_DATA = 1090584578; // 在线转离线无结果

    // 特定业务错误
    public static final int ERROR_SAME_OPERATION = 20; // 相同操作无变化
    public static final int ERROR_RESOURCE = 30; // 资源异常
    public static final int ERROR_OPERATION_CONFLICT = 31; // 业务冲突

    // 未知错误
    public static final int ERROR_UNKNOWN_OPERATION = 16777215; // 操作失败，无具体错误码
    public static final int ERROR_ONLINE_NO_DATA = 1090519040; // 在线数据缺失
    public static final int ERROR_ONLINE_PARSE_FAILED = 1090519041; // 在线数据解析失败

    /**
     * 错误码注解，约束只能使用定义的错误码
     */
    @IntDef({
            ERROR_UNKNOWN,
            ERROR_FAILED,
            ERROR_OK,
            ERROR_DOING,
            ERROR_NOT_INITIALIZED,
            ERROR_INVALID_PARAM,
            ERROR_NULL_PARAM,
            ERROR_NULL_OBJECT,
            ERROR_DB_NOT_READY,
            ERROR_INVALID_FILE_NAME,
            ERROR_FILE_NOT_EXIST,
            ERROR_FILE_UPDATING,
            ERROR_INVALID_PATH_NAME,
            ERROR_PATH_NOT_EXIST,
            ERROR_SET_OFFLINE_MODE,
            ERROR_GUIDE_INIT_FAILED,
            ERROR_INVALID_CONFIG_PATH,
            ERROR_INVALID_CONFIG_FILE,
            ERROR_INVALID_USER,
            ERROR_AUTHENTICATION_FAILED,
            ERROR_INVALID_LON_LAT,
            ERROR_INVALID_RECT,
            ERROR_INVALID_POINT,
            ERROR_ARRAY_EMPTY,
            ERROR_ARRAY_INDEX_OUT_OF_BOUNDS,
            ERROR_NET_CANCEL,
            ERROR_NET_FAILED,
            ERROR_NET_UNREACHABLE,
            ERROR_NET_INVALID_PARAM,
            ERROR_NET_RESPONSE_NULL,
            ERROR_NET_RESPONSE_ERROR,
            ERROR_ZIP,
            ERROR_UNZIP,
            ERROR_OFFLINE_NO_DATA,
            ERROR_ONLINE_TO_OFFLINE_NO_DATA,
            ERROR_SAME_OPERATION,
            ERROR_RESOURCE,
            ERROR_OPERATION_CONFLICT,
            ERROR_UNKNOWN_OPERATION,
            ERROR_ONLINE_NO_DATA,
            ERROR_ONLINE_PARSE_FAILED
    })
    @Retention(RetentionPolicy.SOURCE)
    public @interface ErrorCode {
    }
}