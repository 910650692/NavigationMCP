package com.fy.navi.service.logicpaket.recorder;

import com.fy.navi.service.define.recorder.PlayProgressInfo;

public interface RecorderCallBack {
    void notifyPlayProgress(PlayProgressInfo playProgressInfo);
}
