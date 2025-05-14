package com.fy.navi.service.adapter.activate.cloudpatac;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
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
     */
    public void init() {
        // 设置网络断开时间
        NetConfigUtils.getInstance().setConnectTimeOut(DEFAULT_CONNECT_TIMEOUT);
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "NetConfigUtils is car: " + DeviceUtils.isCar(AppContext.getInstance().getMApplication()));
        if (DeviceUtils.isCar(AppContext.getInstance().getMApplication())) {
            NetPkiFactory.getInstance().init(
                    AppContext.getInstance().getMApplication(),
                    new HeaderBean("SELF_DEVELOPED_MAP", "1.0", 667),
                    "test-ninfo-securitygateway.sgmlink.com",
                    //"ninfo.vapps-qa.sgmlink.com",
                    true
            );
        }
    }
}
