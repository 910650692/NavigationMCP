package com.sgm.navi.service.adapter.activate;

import com.sgm.navi.patacnetlib.NetQueryManager;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.patacnetlib.response.activate.UuidResponse;

public interface IActivateApi {

    /**
     * 检测激活状态
     *
     * @return 是否激活
     */
    boolean checkActivation();

    /**
     * 添加激活观察者
     *
     * @param observer ActivateObserver
     */
    void addActivateObserver(ActivateObserver observer);

    /**
     * 手动激活
     *
     * @param loginCode 激活码
     * @param userCode  序列号
     */
    void manualActivate(String userCode, String loginCode);

    /**
     * 开始激活服务
     */
    void startActivate();

    /**
     * 初始化激活服务
     */
    void init();

    /**
     * 反初始化
     */
    void unInit();

    /**
     * 开始激活流程
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
