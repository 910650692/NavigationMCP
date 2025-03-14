package com.fy.navi.service.adapter.recorder;

import com.fy.navi.service.define.recorder.PlayProgressInfo;

public interface RecorderAdapterCallback {

    void initService();
    // 回放进度通知
    void notifyPlayProgress(PlayProgressInfo playProgressInfo);
}
