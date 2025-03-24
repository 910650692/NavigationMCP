package com.fy.navi.service.adapter.navi;

import com.fy.navi.service.define.navi.SoundInfoEntity;

public interface BaseNaviObserver {
    /**
     * @param info 播报信息
     */
    void onPlayTTS(SoundInfoEntity info);

    /**
     * 导航结束
     */
    void onNaviStop();
}
