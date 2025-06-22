package com.sgm.navi.service.adapter.search.cloudByPatac;

public class NetMethodManager extends com.patac.netlib.factory.NetManager {
    private static volatile NetMethodManager mInstance;
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
    private NetMethodManager(){
        super();
    }
}
