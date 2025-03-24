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
import com.autonavi.gbl.aosclient.observer.ICallBackQRCodeConfirm;
import com.autonavi.gbl.aosclient.observer.ICallBackWsPpAutoWeixinQrcode;
import com.autonavi.gbl.aosclient.observer.ICallBackWsPpAutoWeixinStatus;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.wechat.WeChatAdapterCallBack;
import com.fy.navi.service.define.user.msgpush.PropertyValueInfo;
import com.fy.navi.service.define.user.wechat.BLResponseBean;

import java.util.ArrayList;
import java.util.Hashtable;


public class WeChatImplHelper implements ICallBackWsPpAutoWeixinStatus,
        ICallBackWsPpAutoWeixinQrcode,
        ICallBackQRCodeConfirm{
    private static final String TAG = MapDefaultFinalTag.WE_CHAT_SERVICE_TAG;
    private final Hashtable<String, WeChatAdapterCallBack> mWeChatResultHashtable;
    private BLAosService mBLAosService;

    protected WeChatImplHelper() {
        mWeChatResultHashtable = new Hashtable<>();
    }

    /**
     * 初始化服务
     */
    public void initBLAosService() {
        mBLAosService = new BLAosService();
    }

    /**
     * 注销微信服务
     */
    public void unInitWeChatService() {
        if (mBLAosService != null) {
            mBLAosService = null;
        }
    }

    /**
     * 查询微信服务是否初始化
     * @return 0 未初始化，1 已初始化，2 初始化失败
     */
    public int isInit() {
        if (mBLAosService == null) {
            return -1;
        }
        return mBLAosService.isInit();
    }

    /**
     * 注册微信服务
     * @param key 回调key
     * @param callBack  回调
     */
    public void registerCallBack(final String key, final WeChatAdapterCallBack callBack) {
        mWeChatResultHashtable.put(key, callBack);
    }

    /**
     * 注销微信回调
     * @param key 回调key
     */
    public void unRegisterCallBack(final String key) {
        mWeChatResultHashtable.remove(key);
    }


    /**
     * 移除监听
     */
    public void removeCallback() {
        mWeChatResultHashtable.clear();
    }

    /**
     * 查询是否已经关联微信
     */
    public void sendReqWsPpAutoWeixinStatus() {
        if (mBLAosService == null) {
            return;
        }
        final GWsPpAutoWeixinStatusRequestParam pAosRequest = new GWsPpAutoWeixinStatusRequestParam();
        pAosRequest.product = 1; // 固定值 1  硬件类型（1: 高德车机）
        mBLAosService.sendReqWsPpAutoWeixinStatus(pAosRequest, this);
    }

    /**
     * 获取微信互联二维码
     */
    public void sendReqWsPpAutoWeixinQrcode() {
        if (mBLAosService == null) {
            return;
        }
        final GWsPpAutoWeixinQrcodeRequestParam pAosRequest = new GWsPpAutoWeixinQrcodeRequestParam();
        pAosRequest.product = 1; // 固定值 1   硬件类型（1: 高德车机）
        mBLAosService.sendReqWsPpAutoWeixinQrcode(pAosRequest, this);
    }

    /**
     * 轮询微信是否扫码绑定，该接口需要上层
     * @param qrCodeId 二维码id
     */
    public void sendReqQRCodeConfirm(final String qrCodeId) {
        if (mBLAosService == null) {
            return;
        }
        final GQRCodeConfirmRequestParam pAosRequest = new GQRCodeConfirmRequestParam();
        pAosRequest.QRCodeId = qrCodeId; //  二维码id
        pAosRequest.TypeId = 2; // 二维码类型  绑定微信硬件
        mBLAosService.sendReqQRCodeConfirm(pAosRequest, this);
    }

    /**
     * 二维码登录应答类
     */
    @Override
    public void onRecvAck(final GQRCodeConfirmResponseParam param) {
        if (ConvertUtils.isEmpty(mWeChatResultHashtable)) {
            return;
        }
        for (WeChatAdapterCallBack callBack : mWeChatResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            final BLResponseBean responseBean = getResponseBean(param);
            callBack.notifyGQRCodeConfirm(responseBean);
            Logger.d(TAG,"GQRCodeConfirmResponseParam = " + GsonUtils.toJson(responseBean));
        }
    }

    /**
     * 二维码获取应答类
     */
    @Override
    public void onRecvAck(final GWsPpAutoWeixinQrcodeResponseParam param) {
        if (ConvertUtils.isEmpty(mWeChatResultHashtable)) {
            return;
        }
        for (WeChatAdapterCallBack callBack : mWeChatResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            final BLResponseBean responseBean = getResponseBean(param);
            responseBean.setImgStr(param.imgStr);
            responseBean.setQrcodeId(param.qrcodeId);
            callBack.notifyWeixinQrcode(responseBean);
            Logger.d(TAG,"GWsPpAutoWeixinQrcodeResponseParam = " + GsonUtils.toJson(param));
            sendReqQRCodeConfirm(param.qrcodeId);
        }
    }

    /**
     * 微信激活状态查询应答类
     */
    @Override
    public void onRecvAck(final GWsPpAutoWeixinStatusResponseParam param) {
        if (ConvertUtils.isEmpty(mWeChatResultHashtable)) {
            return;
        }
        for (WeChatAdapterCallBack callBack : mWeChatResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            final BLResponseBean responseBean = getResponseBean(param);
            responseBean.setAvatar(param.avatar);
            responseBean.setNickname(param.nickname);
            callBack.notifyWeixinStatus(responseBean);
            Logger.d(TAG, "GWsPpAutoWeixinStatusResponseParam: " + GsonUtils.toJson(responseBean));
        }
    }


    /**
     * 数据转换
     * @param param 请求对象
     * @return 响应对象
     */
    public BLResponseBean getResponseBean(final BLResponseBase param) {

        final BLResponseBean responseBean = new BLResponseBean();
        responseBean.setEAosRequestType(param.mEAosRequestType);
        responseBean.setNetErrorCode(param.mNetErrorCode);
        responseBean.setNetworkStatus(param.mNetworkStatus);
        responseBean.setReqHandle(param.mReqHandle);
        responseBean.setHttpAckCode(param.mHttpAckCode);
        responseBean.setCode(param.code);
        responseBean.setTimestamp(param.timestamp);
        responseBean.setMessage(param.message);
        responseBean.setVersion(param.version);
        responseBean.setResult(param.result);

        final ArrayList<PropertyValueInfo> propertyValueInfos = new ArrayList<>();
        for (BLKeyValue blKeyValue : param.headers.property) {
            final PropertyValueInfo info = new PropertyValueInfo();
            info.setStrValue(blKeyValue.m_strValue);
            info.setStrKey(blKeyValue.m_strKey);
            propertyValueInfos.add(info);
        }
        responseBean.setHeaders(propertyValueInfos);
        return responseBean;
    }
}
