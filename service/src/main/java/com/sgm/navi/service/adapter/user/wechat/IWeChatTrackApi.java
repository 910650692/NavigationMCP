package com.sgm.navi.service.adapter.user.wechat;

public interface IWeChatTrackApi {

    /**
     * 初始化微信服务
     */
    void initWeChatService();

    /**
     * 注册微信服务
     * @param key 回调key
     * @param callBack  回调
     */
    void registerCallBack(String key, WeChatAdapterCallBack callBack);

    /**
     * 注销微信回调
     * @param key 回调key
     */
    void unRegisterCallback(String key);

    /**
     * 注销微信服务
     */
    void unInitWeChatService();


    /**
     * 查询微信服务是否初始化
     * @return 0 未初始化，1 已初始化，2 初始化失败
     */
    int isInit();

    /**
     * 查询是否已经关联微信
     */
    void sendReqWsPpAutoWeixinStatus();

    /**
     * 获取微信互联二维码
     */
    void sendReqWsPpAutoWeixinQrcode();
}
