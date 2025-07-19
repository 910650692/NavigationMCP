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

    /**
     * 开始激活流程
     *
     * @return AppKey
     */
    String getAppKeyFromDB();

    /**
     * 从网络获取AppKey
     *
     * @param callBack 回调
     */
    void getAppKeyFromNet(final NetQueryManager.INetResultCallBack<AppKeyResponse> callBack);

    /**
     * 从数据库获取UUID
     *
     * @return UUID
     */
    String getUuidFromDB();

    /**
     * 从网络获取UUID
     *
     * @param callBack 回调
     */
    void getUuidFromNet(final NetQueryManager.INetResultCallBack<UuidResponse> callBack);

}
