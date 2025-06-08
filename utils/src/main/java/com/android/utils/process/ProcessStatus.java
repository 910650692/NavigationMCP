package com.android.utils.process;

import androidx.annotation.IntDef;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/5/30
 */
public interface ProcessStatus {
    @IntDef({
            AppRunStatus.DESTROYED,
            AppRunStatus.CREATED,
            AppRunStatus.STARTED,
            AppRunStatus.RESUMED,
            AppRunStatus.PAUSED,
            AppRunStatus.STOPPED
    })

    @interface AppRunStatus {
        int DESTROYED = 0;
        int CREATED = 1;
        int STARTED = 2;
        int RESUMED = 3;
        int PAUSED = 4;
        int STOPPED = 5;
    }
}
