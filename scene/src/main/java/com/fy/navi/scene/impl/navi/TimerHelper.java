package com.fy.navi.scene.impl.navi;

import com.fy.navi.service.define.utils.NumberUtils;

/**
 * 计时辅助类，避免频繁操作UI导致的卡死
 */
public class TimerHelper {
    // 记录最后一次操作时间
    private static long LAST_TIME;

    public static boolean isCanDo() {
        long currentTime = System.currentTimeMillis();
        if (currentTime - LAST_TIME > NumberUtils.NUM_500) {
            LAST_TIME = currentTime;
            return true;
        } else {
            return false;
        }
    }

}
