package com.sgm.navi.service.adapter.user.wechat.bls;

import com.sgm.navi.service.adapter.user.wechat.IWeChatTrackApi;
import com.sgm.navi.service.adapter.user.wechat.WeChatAdapterCallBack;

public class WeChatImpl implements IWeChatTrackApi {

    private WeChatImplHelper mAdapterImplHelper;

    public WeChatImpl() {
        mAdapterImplHelper = new WeChatImplHelper();
    }

    @Override
    public void initWeChatService() {
        mAdapterImplHelper.initBLAosService();
    }

    @Override
    public void registerCallBack(final String key, final WeChatAdapterCallBack callBack) {
        mAdapterImplHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(final String key) {
        mAdapterImplHelper.unRegisterCallBack(key);
    }

    @Override
    public void unInitWeChatService() {
        mAdapterImplHelper.unInitWeChatService();
    }

    /**
     * 获取初始化状态
     * @return 0：未初始化；1：初始化中；2：初始化成功；3：初始化失败
     */
    @Override
    public int isInit() {
       return mAdapterImplHelper.isInit();
    }

    /**
     * 查询是否已经关联微信
     */
    @Override
    public void sendReqWsPpAutoWeixinStatus() {
        mAdapterImplHelper.sendReqWsPpAutoWeixinStatus();
    }

    /**
     * 获取微信互联二维码
     */
    @Override
    public void sendReqWsPpAutoWeixinQrcode() {
        mAdapterImplHelper.sendReqWsPpAutoWeixinQrcode();
    }
}
