package com.fy.navi.service.adapter.user.msgpush;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;

import java.util.ArrayList;
import java.util.Objects;

public class MsgPushAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(MsgPushAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "MsgPushAdapterImpl";
    private IMsgPushApi mMsgPushApi;

    private MsgPushAdapter() {
        mMsgPushApi = (IMsgPushApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initService() {
        mMsgPushApi.initService();
    }

    public void registerCallBack(String key, MsgPushAdapterCallback callBack) {
        mMsgPushApi.registerCallBack(key, callBack);
    }

    public void startListen(String userId) {
        mMsgPushApi.startListen(userId);
    }

    public void stopListen() {
        mMsgPushApi.stopListen();
    }

    public ArrayList<MsgPushInfo> getAutoPushMessages() {
        return mMsgPushApi.getAutoPushMessages();
    }

    public ArrayList<MsgPushInfo> getAimPoiPushMessages() {
        return mMsgPushApi.getAimPoiPushMessages();
    }

    public ArrayList<MsgPushInfo> getAimRoutePushMessages() {
        return mMsgPushApi.getAimRoutePushMessages();
    }

    public void updateAimRouteEndPoiName(long msgId, String msgName) {
        mMsgPushApi.updateAimRouteEndPoiName(msgId, msgName);
    }

    public long sendReqSendToPhone(MsgPushRequestInfo pAosRequest) {
        return mMsgPushApi.sendReqSendToPhone(pAosRequest);
    }

    public MsgPushResponseInfo request(long deviceId, GeoPoint userLocation) {
        return mMsgPushApi.request(deviceId,userLocation);
    }

    public void abort() {
        mMsgPushApi.abort();
    }

    public void abort(long taskId) {
        mMsgPushApi.abort(taskId);
    }

    public static MsgPushAdapter getInstance() {
        return Helper.mpa;
    }

    public long sendReqWsTserviceInternalLinkAutoReport (MsgPushRequestInfo pAosRequest) {
        return mMsgPushApi.sendReqWsTserviceInternalLinkAutoReport(pAosRequest);
    }

    private static final class Helper {
        private static final MsgPushAdapter mpa = new MsgPushAdapter();
    }
}
