package com.fy.navi.service.logicpaket.user.wechat;

import com.fy.navi.service.define.user.wechat.BLResponseBean;


public interface WeChatCallBack {

    /**
     * 微信二维码扫码轮询
     * @param result 回调数据
     */
    default void notifyGQRCodeConfirm(BLResponseBean result){

    }

    /**
     * 微信二维码请求回调
     * @param result  回调数据
     */
    default void notifyWeixinQrcode(BLResponseBean result){

    }

    /**
     * 微信登陆状态请求回调
     * @param result  回调数据
     */
    default void notifyWeixinStatus(BLResponseBean result){

    }
}
