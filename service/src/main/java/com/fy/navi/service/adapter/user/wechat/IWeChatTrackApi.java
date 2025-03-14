package com.fy.navi.service.adapter.user.wechat;

/**
 * @Description
 * @Author fh
 * @date 2024/12/30
 */
public interface IWeChatTrackApi {

    void initWeChatService();

    void registerCallBack(String key, WeChatAdapterCallBack callBack);

    void unRegisterCallback(String key);

    void unInitWeChatService();

    int isInit();

    /**
     * 查询是否已经关联微信
     */
    void sendReqWsPpAutoWeixinStatus();

    /**
     * 获取微信互联二维码
     */
    void sendReqWsPpAutoWeixinQrcode();

    /**
     * 轮询微信是否扫码绑定，该接口需要上层
     */
    void sendReqQRCodeConfirm(String QRCodeId);

    /**
     * 解除微信互联
     */
    void sendReqWsPpAutoWeixinUnbind();

}
