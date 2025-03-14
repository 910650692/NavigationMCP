package com.fy.navi.service.adapter.user.wechat;

import com.fy.navi.service.AdapterConfig;

import java.util.Objects;

/**
 * @Description
 * @Author fh
 * @date 2024/12/31
 */
public class WeChatAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(WeChatAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "WeChatImpl";
    private IWeChatTrackApi mWeChatApi;

    private WeChatAdapter() {
        mWeChatApi = (IWeChatTrackApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initWeChatService() {
        mWeChatApi.initWeChatService();
    }

    public void registerCallBack(String key, WeChatAdapterCallBack callBack) {
        mWeChatApi.registerCallBack(key, callBack);
    }

    public void unInitWeChatService() {
        mWeChatApi.unInitWeChatService();
    }

    public void sendReqWsPpAutoWeixinStatus() {
        mWeChatApi.sendReqWsPpAutoWeixinStatus();
    }

    public void sendReqWsPpAutoWeixinQrcode() {
        mWeChatApi.sendReqWsPpAutoWeixinQrcode();
    }

    public void sendReqQRCodeConfirm(String QRCodeId) {
        mWeChatApi.sendReqQRCodeConfirm(QRCodeId);
    }

    public void sendReqWsPpAutoWeixinUnbind() {
        mWeChatApi.sendReqWsPpAutoWeixinUnbind();
    }

    public static WeChatAdapter getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final WeChatAdapter ra = new WeChatAdapter();
    }
}
