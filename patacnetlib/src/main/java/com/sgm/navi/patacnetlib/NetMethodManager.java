package com.sgm.navi.patacnetlib;

import com.patac.netlib.factory.NetManager;

public final class NetMethodManager extends NetManager {
    private static volatile NetMethodManager mInstance;

    private NetMethodManager() {
    }

    private static final class SingleHolder {
        private static final NetMethodManager INSTANCE = new NetMethodManager();
    }

    /**
     * 获取单例
     *
     * @return 单例
     */
    public static NetMethodManager getInstance() {
        return SingleHolder.INSTANCE;
    }
}
