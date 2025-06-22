package com.sgm.navi.service.adapter.navi;

import com.sgm.navi.service.define.navi.SoundInfoEntity;

public interface BaseNaviObserver {
    /**
     * @param info 播报信息
     */
    void onPlayTTS(SoundInfoEntity info);

    /**
     * @param type 导航叮叮音播报
     */
    void onPlayRing(int type);

    /**
     * 导航结束
     */
    void onNaviStop();
}
