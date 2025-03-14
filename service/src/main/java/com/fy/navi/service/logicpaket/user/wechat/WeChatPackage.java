package com.fy.navi.service.logicpaket.user.wechat;


import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.wechat.WeChatAdapter;
import com.fy.navi.service.adapter.user.wechat.WeChatAdapterCallBack;
import com.fy.navi.service.define.user.wechat.BLResponseBean;

import java.util.Hashtable;

/**
 * @Description
 * @Author fh
 * @date 2024/12/31
 */
public class WeChatPackage implements WeChatAdapterCallBack {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private final WeChatAdapter mWeChatAdapter;
    private final Hashtable<String, WeChatCallBack> callBacks;

    private WeChatPackage() {
        callBacks = new Hashtable<>();
        mWeChatAdapter = WeChatAdapter.getInstance();
    }

    public void initWeChatService() {
        mWeChatAdapter.initWeChatService();
        mWeChatAdapter.registerCallBack("WeChatPackage", this);
    }

    public synchronized void registerCallBack(String key, WeChatCallBack callback) {
        if (callback != null && !callBacks.contains(callback)) {
            callBacks.put(key, callback);
        }
    }

    public void unRegisterCallBack(String callback) {
        if (callback == null) {
            return;
        }
        callBacks.remove(callback);
    }

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

    /**
     * 轮询微信是否扫码绑定，该接口需要上层
     */
    public void sendReqQRCodeConfirm(String QRCodeId) {
        mWeChatAdapter.sendReqQRCodeConfirm(QRCodeId);
    }

    /**
     * 解除微信互联
     */
    public void sendReqWsPpAutoWeixinUnbind() {
        mWeChatAdapter.sendReqWsPpAutoWeixinUnbind();
    }


    public static WeChatPackage getInstance() {
        return Helper.ep;
    }

    /**
     * 微信二维码扫码轮询
     * @param result 回调数据
     */
    @Override
    public void notifyGQRCodeConfirm(BLResponseBean result) {
        for (WeChatCallBack observer : callBacks.values()) {
            observer.notifyGQRCodeConfirm(result);
        }
    }

    /**
     * 微信二维码请求回调
     * @param result 回调数据
     */
    @Override
    public void notifyWeixinQrcode(BLResponseBean result) {
        for (WeChatCallBack observer : callBacks.values()) {
            observer.notifyWeixinQrcode(result);
        }
    }

    /**
     * 微信登陆状态请求回调
     * @param result 回调数据
     */
    @Override
    public void notifyWeixinStatus(BLResponseBean result) {
        for (WeChatCallBack observer : callBacks.values()) {
            observer.notifyWeixinStatus(result);
        }
    }

    private static final class Helper {
        private static final WeChatPackage ep = new WeChatPackage();
    }

}
