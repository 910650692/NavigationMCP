package com.sgm.navi.service.adapter.user.wechat;

import com.sgm.navi.service.AdapterConfig;

import java.util.Objects;


public final class WeChatAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(WeChatAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "WeChatImpl";
    private IWeChatTrackApi mWeChatApi;

    private WeChatAdapter() {
        mWeChatApi = (IWeChatTrackApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    /**
     * 初始化微信服务
     */
    public void initWeChatService() {
        mWeChatApi.initWeChatService();
    }


    /**
     * 注册微信服务
     * @param key 回调key
     * @param callBack  回调
     */
    public void registerCallBack(final String key, final WeChatAdapterCallBack callBack) {
        mWeChatApi.registerCallBack(key, callBack);
    }


    /**
     * 注销微信服务
     */
    public void unInitWeChatService() {
        mWeChatApi.unInitWeChatService();
    }

    /**
     * 查询是否已经关联微信
     */
    public void sendReqWsPpAutoWeixinStatus() {
        mWeChatApi.sendReqWsPpAutoWeixinStatus();
    }

    /**
     * 获取微信互联二维码
     */
    public void sendReqWsPpAutoWeixinQrcode() {
        mWeChatApi.sendReqWsPpAutoWeixinQrcode();
    }

    public static WeChatAdapter getInstance() {
        return Helper.RA;
    }

    private static final class Helper {
        private static final WeChatAdapter RA = new WeChatAdapter();
    }
}
