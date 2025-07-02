package com.sgm.navi.service.adapter.activate;

import com.sgm.navi.patacnetlib.NetQueryManager;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.patacnetlib.response.activate.UuidResponse;

public interface ActivateObserver {

    /**
     * 正在激活
     */
    void onActivating();

    /**
     * 网络激活失败
     * @param failedCount 网络激活失败次数
     */
    void onNetActivateFailed(int failedCount);

    /**
     * 激活成功
     */
    void onActivated();

    /**
     * 激活出错
     *
     * @param msg 错误信息
     * @param errCode 错误码
     */
    void onActivatedError(int errCode, String msg);

    String getAppKeyFromDB();

    void getAppKeyFromNet(final NetQueryManager.INetResultCallBack<AppKeyResponse> callBack);

    String getUuidFromDB();

    void getUuidFromNet(final NetQueryManager.INetResultCallBack<UuidResponse> callBack);

}
