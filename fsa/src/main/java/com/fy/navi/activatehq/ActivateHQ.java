package com.fy.navi.activatehq;

import com.fy.navi.service.MapDefaultFinalTag;
import com.gm.cn.adassdk.AdasManager;

public final class ActivateHQ {
    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;
    private AdasManager mAdasManager;
    private ActivateHQ() {

    }

    private static final class SingleHolder {
        public static final ActivateHQ INSTANCE = new ActivateHQ();
    }

    /**
     * 获取实例
     *
     * @return ActivateHQ
     */
    public static ActivateHQ getInstance() {
        return SingleHolder.INSTANCE;
    }

    /**
     * 初始化adasManager，同时也是hq激活接口
     *
     * @param manager adasManager
     */
    public void init(final AdasManager manager) {
        this.mAdasManager = manager;
        startActivate();
    }

    /**
     * 开始激活
     */
    private void startActivate() {

    }


}
