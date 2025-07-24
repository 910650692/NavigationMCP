package com.sgm.navi.service.adapter.activate;

import com.sgm.navi.patacnetlib.NetQueryManager;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.patacnetlib.response.activate.UuidResponse;
import com.sgm.navi.service.AdapterConfig;

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
     * 开始激活服务
     */
    public void startActivate() {
        mActivateApi.startActivate();
    }

    /**
     * 初始化激活服务
     */
    public void init() {
        mActivateApi.init();
    }

    /**
     * 反初始化激活服务
     */
    public void unInit() {
        mActivateApi.unInit();
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

    /**
     * 从数据库获取AppKey
     *
     * @return AppKey
     */
    public String getAppKeyFromDB() {
        return mActivateApi.getAppKeyFromDB();
    }

    /**
     * 从网络获取AppKey
     *
     * @param callBack 回调
     */
    public void getAppKeyFromNet(final NetQueryManager.INetResultCallBack<AppKeyResponse> callBack) {
        mActivateApi.getAppKeyFromNet(callBack);
    }

    /**
     * 从数据库获取UUID
     *
     * @return UUID
     */
    public String getUuidFromDB() {
        return mActivateApi.getUuidFromDB();
    }

    /**
     * 从网络获取UUID
     *
     * @param callBack 回调
     */
    public void getUuidFromNet(final NetQueryManager.INetResultCallBack<UuidResponse> callBack) {
        mActivateApi.getUuidFromNet(callBack);
    }

}
