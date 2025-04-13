package com.fy.navi.service.define.engine;

/**
 * @Description:
 * @author: chao.tang
 * @date: 2025年04月10日 15:55
 */
public enum GaodeLogLevel {

    LOG_NONE(0L),

    LOG_VERBOSE(128L),
    LOG_DEBUG(8L),
    LOG_INFO(16L),
    LOG_WARN(32L),
    LOG_ERROR(64L);

    // 用于存储枚举常量对应的数字值
    private final long code;

    // 枚举的构造函数，用于初始化数字值
    GaodeLogLevel(long code) {
        this.code = code;
    }

    // 获取枚举常量对应的数字值的方法
    public long getCode() {
        return code;
    }
}
