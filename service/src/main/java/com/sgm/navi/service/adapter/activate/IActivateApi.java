package com.sgm.navi.service.adapter.activate;

import com.sgm.navi.patacnetlib.NetQueryManager;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.patacnetlib.response.activate.UuidResponse;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.adapter.activate.bls.ActivationManager;
import com.sgm.navi.service.greendao.CommonManager;

public interface IActivateApi {

    /**
     * 检测激活状态
     * @return 是否激活
     */
    boolean checkActivation();

    /**
     * 添加激活观察者
     * @param observer ActivateObserver
     */
    void addActivateObserver(ActivateObserver observer);

    /**
     * 重试网络激活
     */
    void netActivateRetry();

    /**
     * 手动激活
     * @param loginCode 激活码
     * @param userCode 序列号
     */
    void manualActivate(String userCode, String loginCode);

    /**
     * 初始化激活服务
     */
    void initActivate();

    /**
     * 反初始化
     */
    void unInit();

    String getAppKeyFromDB();

    void getAppKeyFromNet(final NetQueryManager.INetResultCallBack<AppKeyResponse> callBack);

    String getUuidFromDB();

    void getUuidFromNet(final NetQueryManager.INetResultCallBack<UuidResponse> callBack);
}
