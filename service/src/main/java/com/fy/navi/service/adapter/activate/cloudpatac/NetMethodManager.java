package com.fy.navi.service.adapter.activate.cloudpatac;

public class NetMethodManager extends com.patac.netlib.factory.NetManager {
    private static volatile NetMethodManager mInstance;

    /**
     * 获取单例
     * @return 单例
     */
    public static NetMethodManager getInstance() {
        if (null == mInstance) {
            synchronized (NetMethodManager.class) {
                if (null == mInstance) {
                    mInstance = new NetMethodManager();
                }
            }
        }
        return mInstance;
    }
}
