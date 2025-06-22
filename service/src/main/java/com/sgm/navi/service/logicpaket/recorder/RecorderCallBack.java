package com.sgm.navi.service.logicpaket.recorder;

import com.sgm.navi.service.define.recorder.PlayProgressInfo;

public interface RecorderCallBack {
    void notifyPlayProgress(PlayProgressInfo playProgressInfo);
}
