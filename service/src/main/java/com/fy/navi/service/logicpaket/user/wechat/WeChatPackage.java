package com.fy.navi.service.logicpaket.user.wechat;


import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.wechat.WeChatAdapter;
import com.fy.navi.service.adapter.user.wechat.WeChatAdapterCallBack;
import com.fy.navi.service.define.user.wechat.BLResponseBean;

import java.util.Hashtable;


public final class WeChatPackage implements WeChatAdapterCallBack {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private final WeChatAdapter mWeChatAdapter;
    private final Hashtable<String, WeChatCallBack> mCallBacks;

    private WeChatPackage() {
        mCallBacks = new Hashtable<>();
        mWeChatAdapter = WeChatAdapter.getInstance();
    }

    /**
     * 初始化微信服务
     */
    public void initWeChatService() {
        mWeChatAdapter.initWeChatService();
        mWeChatAdapter.registerCallBack("WeChatPackage", this);
    }

    /**
     * 注册微信回调
     * @param key 回调key
     * @param callback 回调
     */
    public synchronized void registerCallBack(final String key, final WeChatCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.put(key, callback);
        }
    }

    /**
     * 注销微信回调
     * @param callback 回调
     */
    public void unRegisterCallBack(final String callback) {
        if (callback == null) {
            return;
        }
        mCallBacks.remove(callback);
    }

    /**
     * 注销微信服务
     */
    public void unInitWeChatService() {
        mWeChatAdapter.unInitWeChatService();
    }

    /**
     * 查询是否已经关联微信
     */
    public void sendReqWsPpAutoWeixinStatus() {
        mWeChatAdapter.sendReqWsPpAutoWeixinStatus();
    }

    /**
     * 获取微信互联二维码
     */
    public void sendReqWsPpAutoWeixinQrcode() {
        mWeChatAdapter.sendReqWsPpAutoWeixinQrcode();
    }

    public static WeChatPackage getInstance() {
        return Helper.EP;
    }

    /**
     * 微信二维码扫码轮询
     * @param result 回调数据
     */
    @Override
    public void notifyGQRCodeConfirm(final BLResponseBean result) {
        for (WeChatCallBack observer : mCallBacks.values()) {
            observer.notifyGQRCodeConfirm(result);
        }
    }

    /**
     * 微信二维码请求回调
     * @param result 回调数据
     */
    @Override
    public void notifyWeixinQrcode(final BLResponseBean result) {
        for (WeChatCallBack observer : mCallBacks.values()) {
            observer.notifyWeixinQrcode(result);
        }
    }

    /**
     * 微信登陆状态请求回调
     * @param result 回调数据
     */
    @Override
    public void notifyWeixinStatus(final BLResponseBean result) {
        for (WeChatCallBack observer : mCallBacks.values()) {
            observer.notifyWeixinStatus(result);
        }
    }

    private static final class Helper {
        private static final WeChatPackage EP = new WeChatPackage();
    }

}
