package com.fy.navi.service.adapter.user.msgpush;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;

import java.util.ArrayList;
import java.util.Objects;

final public class MsgPushAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(MsgPushAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "MsgPushAdapterImpl";
    private IMsgPushApi mMsgPushApi;

    private MsgPushAdapter() {
        mMsgPushApi = (IMsgPushApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    /**
     * initService
     */
    public void initService() {
        mMsgPushApi.initService();
    }

    /**
     * registerCallBack
     * @param key
     * @param callBack
     */
    public void registerCallBack(final String key, final MsgPushAdapterCallback callBack) {
        mMsgPushApi.registerCallBack(key, callBack);
    }

    /**
     * startListen
     * @param userId
     */
    public void startListen(final String userId) {
        mMsgPushApi.startListen(userId);
    }

    /**
     * stopListen
     */
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

    /**
     * updateAimRouteEndPoiName
     * @param msgId
     * @param msgName
     */
    public void updateAimRouteEndPoiName(final long msgId, final String msgName) {
        mMsgPushApi.updateAimRouteEndPoiName(msgId, msgName);
    }

    /**
     * sendReqSendToPhone
     * @param aosRequest
     * @return long
     */
    public long sendReqSendToPhone(final MsgPushRequestInfo aosRequest) {
        return mMsgPushApi.sendReqSendToPhone(aosRequest);
    }

    /**
     * request
     * @param deviceId
     * @param userLocation
     * @return MsgPushResponseInfo
     */
    public MsgPushResponseInfo request(final long deviceId, final GeoPoint userLocation) {
        return mMsgPushApi.request(deviceId,userLocation);
    }

    /**
     * abort
     */
    public void abort() {
        mMsgPushApi.abort();
    }

    /**
     * abort
     * @param taskId
     */
    public void abort(final long taskId) {
        mMsgPushApi.abort(taskId);
    }

    public static MsgPushAdapter getInstance() {
        return Helper.MPA;
    }

    /**
     * sendReqWsTserviceInternalLinkAutoReport
     * @param aosRequest
     * @return long
     */
    public long sendReqWsTserviceInternalLinkAutoReport(final MsgPushRequestInfo aosRequest) {
        return mMsgPushApi.sendReqWsTserviceInternalLinkAutoReport(aosRequest);
    }

    private static final class Helper {
        private static final MsgPushAdapter MPA = new MsgPushAdapter();
    }
}
