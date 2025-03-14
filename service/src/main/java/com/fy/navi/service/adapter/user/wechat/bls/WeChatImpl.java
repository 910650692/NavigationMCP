package com.fy.navi.service.adapter.user.wechat.bls;

import com.fy.navi.service.adapter.user.wechat.IWeChatTrackApi;
import com.fy.navi.service.adapter.user.wechat.WeChatAdapterCallBack;

/**
 * 高德 车主服务 - 微信互联.
 *
 * @Description Impl类只做SDK的原子能力封装，不做对象及数据转换
 * @Author fh
 * @date 2024/12/26
 */
public class WeChatImpl implements IWeChatTrackApi {
    private WeChatImplHelper adapterImplHelper;

    public WeChatImpl() {
        adapterImplHelper = new WeChatImplHelper();
    }

    @Override
    public void initWeChatService() {
        adapterImplHelper.initBLAosService();
    }

    @Override
    public void registerCallBack(String key, WeChatAdapterCallBack callBack) {
        adapterImplHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(String key) {
        adapterImplHelper.unRegisterCallBack(key);
    }

    @Override
    public void unInitWeChatService() {
        adapterImplHelper.unInitWeChatService();
    }

    /**
     * 获取初始化状态
     * @return
     */
    @Override
    public int isInit() {
       return adapterImplHelper.isInit();
    }

    /**
     * 查询是否已经关联微信
     */
    @Override
    public void sendReqWsPpAutoWeixinStatus() {
        adapterImplHelper.sendReqWsPpAutoWeixinStatus();
    }

    /**
     * 获取微信互联二维码
     */
    @Override
    public void sendReqWsPpAutoWeixinQrcode() {
        adapterImplHelper.sendReqWsPpAutoWeixinQrcode();
    }

    /**
     * 轮询微信是否扫码绑定，该接口需要上层
     */
    @Override
    public void sendReqQRCodeConfirm(String QRCodeId) {
        adapterImplHelper.sendReqQRCodeConfirm(QRCodeId);
    }

    /**
     * 解除微信互联
     */
    @Override
    public void sendReqWsPpAutoWeixinUnbind() {
        adapterImplHelper.sendReqWsPpAutoWeixinUnbind();
    }

}
