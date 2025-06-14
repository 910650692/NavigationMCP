package com.fy.navi.patacnetlib;


import android.app.Application;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.patacnetlib.api.NetApiHelper;
import com.patac.netlib.bean.HeaderBean;
import com.patac.netlib.factory.NetPkiFactory;
import com.patac.netlib.utils.NetConfigUtils;

public final class PatacNetClient {
    private static volatile PatacNetClient mInstance;
    private static final int DEFAULT_CONNECT_TIMEOUT = 15;// SECONDS

    /**
     * Init client
     *
     * @return instance
     */
    public static PatacNetClient getInstance() {
        if (null == mInstance) {
            synchronized (PatacNetClient.class) {
                if (null == mInstance) {
                    mInstance = new PatacNetClient();
                }
            }
        }
        return mInstance;
    }

    private PatacNetClient() {
    }

    /**
     * init port:测试环境667 生产668
     *
     * @param application app
     */
    public void init(final Application application) {
        // 设置网络断开时间
        NetConfigUtils.getInstance().setConnectTimeOut(DEFAULT_CONNECT_TIMEOUT);
        Logger.d(NetApiHelper.ACTIVATE_TAG, "NetConfigUtils is car: " + DeviceUtils.isCar(application));
        if (DeviceUtils.isCar(application)) {
            NetPkiFactory.getInstance().init(
                    application,
                    new HeaderBean("SELF_DEVELOPED_MAP", "1.0", 668),
                    NetApiHelper.DOMAIN,
                    true
            );
        }
    }
}
