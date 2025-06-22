package com.sgm.navi.service.adapter.activate;

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
}
