package com.fy.navi.service.adapter.activate;

import com.fy.navi.service.AdapterConfig;

public final class ActivateAdapter {

    private static final String ACTIVATE_API_PKG = ActivateAdapter.class.getPackage().getName();
    private static final String ACTIVATE_API_CLS = "ActivateAdapterImpl";

    private IActivateApi mActivateApi;

    private ActivateAdapter() {
        mActivateApi = (IActivateApi) AdapterConfig.getObject(ACTIVATE_API_PKG, ACTIVATE_API_CLS);
    }

    private static final class Helper {
        private static final ActivateAdapter INSTANCE = new ActivateAdapter();
    }

    public static ActivateAdapter getInstance() {
        return Helper.INSTANCE;
    }

    public boolean checkActivation() {
        return mActivateApi.checkActivation();
    }

    /**
     * 初始化激活服务
     */
    public void initActivate() {
        mActivateApi.initActivate();
    }

    /**
     * 反初始化激活服务
     */
    public void unInit() {
        mActivateApi.unInit();
    }

    /**
     * 重试网络激活
     */
    public void netActivateRetry() {
        mActivateApi.netActivateRetry();
    }

    /**
     * 手动激活
     *
     * @param loginCode 激活码
     * @param userCode  序列号
     */
    public void manualActivate(final String userCode, final String loginCode) {
        mActivateApi.manualActivate(userCode, loginCode);
    }

    /**
     * 添加激活观察者
     *
     * @param observer ActivateObserver
     */
    public void addActivateObserver(final ActivateObserver observer) {
        mActivateApi.addActivateObserver(observer);
    }

}
