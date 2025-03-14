package com.fy.navi.service.adapter.navi;

import com.fy.navi.service.define.navi.SoundInfoEntity;

public interface BaseNaviObserver {
    /*播报信息*/
    void onPlayTTS(SoundInfoEntity pInfo);

    /*导航结束*/
    void onNaviStop();
}
