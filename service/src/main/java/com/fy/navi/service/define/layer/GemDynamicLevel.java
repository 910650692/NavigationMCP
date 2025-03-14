package com.fy.navi.service.define.layer;

import androidx.annotation.IntDef;

/**
 * Author: QiuYaWei
 * Date: 2025/3/7
 * Description: [在这里描述文件功能]
 */
public class GemDynamicLevel {
    public static final int NAVI = 0;
    public static final int CRUISE = 1;

    @IntDef({NAVI, CRUISE})
    public @interface GemDynamicLevelType {

    }
}
