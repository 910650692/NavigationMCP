package com.fy.navi.service.adapter.user.wechat.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.BLKeyValue;
import com.autonavi.gbl.aosclient.model.BLResponseBase;
import com.autonavi.gbl.aosclient.model.GQRCodeConfirmRequestParam;
import com.autonavi.gbl.aosclient.model.GQRCodeConfirmResponseParam;
import com.autonavi.gbl.aosclient.model.GWsPpAutoWeixinQrcodeRequestParam;
import com.autonavi.gbl.aosclient.model.GWsPpAutoWeixinQrcodeResponseParam;
import com.autonavi.gbl.aosclient.model.GWsPpAutoWeixinStatusRequestParam;
import com.autonavi.gbl.aosclient.model.GWsPpAutoWeixinStatusResponseParam;
import com.autonavi.gbl.aosclient.model.GWsPpAutoWeixinUnbindRequestParam;
import com.autonavi.gbl.aosclient.model.GWsPpAutoWeixinUnbindResponseParam;
import com.autonavi.gbl.aosclient.observer.ICallBackQRCodeConfirm;
import com.autonavi.gbl.aosclient.observer.ICallBackWsPpAutoWeixinQrcode;
import com.autonavi.gbl.aosclient.observer.ICallBackWsPpAutoWeixinStatus;
import com.autonavi.gbl.aosclient.observer.ICallBackWsPpAutoWeixinUnbind;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.wechat.WeChatAdapterCallBack;
import com.fy.navi.service.define.user.msgpush.PropertyValueInfo;
import com.fy.navi.service.define.user.wechat.BLResponseBean;

import java.util.ArrayList;
import java.util.Hashtable;

/**
 * BLAosService 辅助类.
 *
 * @Description Helper类只做对象及数据转换，不做原子能力调用
 * @Author fh
 * @date 2024/12/31
 */
public class WeChatImplHelper implements ICallBackWsPpAutoWeixinStatus, ICallBackWsPpAutoWeixinQrcode, ICallBackQRCodeConfirm, ICallBackWsPpAutoWeixinUnbind {
    private static final String TAG = MapDefaultFinalTag.WE_CHAT_SERVICE_TAG;
    private final Hashtable<String, WeChatAdapterCallBack> weChatResultHashtable;
    private BLAosService mBLAosService;

    protected WeChatImplHelper() {
        weChatResultHashtable = new Hashtable<>();
    }

    /**
     * 初始化服务
     */
    public void initBLAosService() {
        mBLAosService = new BLAosService();
    }

    public void unInitWeChatService() {
        if (mBLAosService != null) {
            mBLAosService = null;
        }
    }

    public int isInit() {
        if (mBLAosService == null) return -1;
        return mBLAosService.isInit();
    }

    public void registerCallBack(String key, WeChatAdapterCallBack callBack) {
        weChatResultHashtable.put(key, callBack);
    }

    public void unRegisterCallBack(String key) {
        weChatResultHashtable.remove(key);
    }

    public void removeCallback() {
        weChatResultHashtable.clear();
    }

    /**
     * 查询是否已经关联微信
     */
    public void sendReqWsPpAutoWeixinStatus() {
        if (mBLAosService == null) return;
        GWsPpAutoWeixinStatusRequestParam pAosRequest = new GWsPpAutoWeixinStatusRequestParam();
        pAosRequest.product = 1; // 固定值 1  硬件类型（1: 高德车机）
        mBLAosService.sendReqWsPpAutoWeixinStatus(pAosRequest, this);
    }

    /**
     * 获取微信互联二维码
     */
    public void sendReqWsPpAutoWeixinQrcode() {
        if (mBLAosService == null) return;
        GWsPpAutoWeixinQrcodeRequestParam pAosRequest = new GWsPpAutoWeixinQrcodeRequestParam();
        pAosRequest.product = 1; // 固定值 1   硬件类型（1: 高德车机）
        mBLAosService.sendReqWsPpAutoWeixinQrcode(pAosRequest, this);
    }

    /**
     * 轮询微信是否扫码绑定，该接口需要上层
     */
    public void sendReqQRCodeConfirm(String QRCodeId) {
        if (mBLAosService == null) return;
        GQRCodeConfirmRequestParam pAosRequest = new GQRCodeConfirmRequestParam();
        pAosRequest.QRCodeId = QRCodeId; //  二维码id
        pAosRequest.TypeId = 2; // 二维码类型  绑定微信硬件
        mBLAosService.sendReqQRCodeConfirm(pAosRequest, this);
    }

    /**
     * 解除微信互联
     */
    public void sendReqWsPpAutoWeixinUnbind() {
        if (mBLAosService == null) return;
        GWsPpAutoWeixinUnbindRequestParam pAosRequest = new GWsPpAutoWeixinUnbindRequestParam();
        pAosRequest.product = 1; // 固定值 1   硬件类型（1: 高德车机）
        mBLAosService.sendReqWsPpAutoWeixinUnbind(pAosRequest, this);
    }

    /**
     * 二维码登录应答类
     */
    @Override
    public void onRecvAck(GQRCodeConfirmResponseParam param) {
        if (ConvertUtils.isEmpty(weChatResultHashtable)) return;
        for (WeChatAdapterCallBack callBack : weChatResultHashtable.values()) {
            if (callBack == null) continue;
            BLResponseBean responseBean = getResponseBean(param);
            callBack.notifyGQRCodeConfirm(responseBean);
            Logger.d(TAG,"GQRCodeConfirmResponseParam = " + GsonUtils.toJson(responseBean));
        }
    }

    /**
     * 二维码获取应答类
     */
    @Override
    public void onRecvAck(GWsPpAutoWeixinQrcodeResponseParam param) {
        if (ConvertUtils.isEmpty(weChatResultHashtable)) return;
        for (WeChatAdapterCallBack callBack : weChatResultHashtable.values()) {
            if (callBack == null) continue;
            BLResponseBean responseBean = getResponseBean(param);
            GsonUtils.copyBean(param, responseBean);
            callBack.notifyWeixinQrcode(responseBean);
            Logger.d(TAG,"GWsPpAutoWeixinQrcodeResponseParam = " + GsonUtils.toJson(param));
            sendReqQRCodeConfirm(param.qrcodeId);
        }
    }

    /**
     * 微信激活状态查询应答类
     */
    @Override
    public void onRecvAck(GWsPpAutoWeixinStatusResponseParam param) {
        if (ConvertUtils.isEmpty(weChatResultHashtable)) return;
        for (WeChatAdapterCallBack callBack : weChatResultHashtable.values()) {
            if (callBack == null) continue;
            BLResponseBean responseBean = getResponseBean(param);
            GsonUtils.copyBean(param, responseBean);
            callBack.notifyWeixinStatus(responseBean);
            Logger.d(TAG, "GWsPpAutoWeixinStatusResponseParam: " + GsonUtils.toJson(responseBean));
        }
    }

    /**
     * 关闭微信互联功能应答类
     */
    @Override
    public void onRecvAck(GWsPpAutoWeixinUnbindResponseParam param) {
        if (ConvertUtils.isEmpty(weChatResultHashtable)) return;
        for (WeChatAdapterCallBack callBack : weChatResultHashtable.values()) {
            if (callBack == null) continue;
            BLResponseBean responseBean = getResponseBean(param);
            Logger.d(TAG,"GWsPpAutoWeixinUnbindResponseParam = " + GsonUtils.toJson(responseBean));
        }
    }

    public BLResponseBean getResponseBean(BLResponseBase param) {

        BLResponseBean responseBean = new BLResponseBean();
        responseBean.setmEAosRequestType(param.mEAosRequestType);
        responseBean.setmNetErrorCode(param.mNetErrorCode);
        responseBean.setmNetworkStatus(param.mNetworkStatus);
        responseBean.setmReqHandle(param.mReqHandle);
        responseBean.setmHttpAckCode(param.mHttpAckCode);
        responseBean.setCode(param.code);
        responseBean.setTimestamp(param.timestamp);
        responseBean.setMessage(param.message);
        responseBean.setVersion(param.version);
        responseBean.setResult(param.result);

        ArrayList<PropertyValueInfo> propertyValueInfos = new ArrayList<>();
        for (BLKeyValue blKeyValue : param.headers.property) {
            PropertyValueInfo info = new PropertyValueInfo();
            info.setmStrValue(blKeyValue.m_strValue);
            info.setmStrKey(blKeyValue.m_strKey);
            propertyValueInfos.add(info);
        }
        responseBean.setHeaders(propertyValueInfos);
        return responseBean;
    }
}
