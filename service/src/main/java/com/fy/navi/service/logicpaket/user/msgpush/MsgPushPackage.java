package com.fy.navi.service.logicpaket.user.msgpush;

import com.fy.navi.service.adapter.user.msgpush.MsgPushAdapter;
import com.fy.navi.service.adapter.user.msgpush.MsgPushAdapterCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushResponseInfo;

import java.util.ArrayList;
import java.util.Hashtable;


public class MsgPushPackage implements MsgPushAdapterCallback{
    private final MsgPushAdapter msgPushAdapter;
    private final Hashtable<String, MsgPushCallBack> callBacks;

    public MsgPushPackage() {
        callBacks = new Hashtable<>();
        msgPushAdapter = MsgPushAdapter.getInstance();

    }

    public static MsgPushPackage getInstance() {
        return SInstanceHolder.sInstance;
    }

    public void startListen(String userId) {
        msgPushAdapter.startListen(userId);
    }

    public void stopListen() {
        msgPushAdapter.stopListen();
    }

    public ArrayList<MsgPushInfo> getAutoPushMessages() {
        return msgPushAdapter.getAutoPushMessages();
    }

    public ArrayList<MsgPushInfo> getAimPoiPushMessages() {
        return msgPushAdapter.getAimPoiPushMessages();
    }

    public ArrayList<MsgPushInfo> getAimRoutePushMessages() {
        return msgPushAdapter.getAimRoutePushMessages();
    }

    public void updateAimRouteEndPoiName(long msgId, String msgName) {
        msgPushAdapter.updateAimRouteEndPoiName(msgId, msgName);
    }

    public long sendReqSendToPhone(MsgPushRequestInfo pAosRequest) {
        return msgPushAdapter.sendReqSendToPhone(pAosRequest);
    }

    public MsgPushResponseInfo request(long deviceId, GeoPoint userLocation) {
        return msgPushAdapter.request(deviceId, userLocation);
    }

    public void abort() {
        msgPushAdapter.abort();
    }

    public void abort(long taskId) {
        msgPushAdapter.abort(taskId);
    }

    public long sendReqWsTserviceInternalLinkAutoReport(MsgPushRequestInfo pAosRequest) {
        return msgPushAdapter.sendReqWsTserviceInternalLinkAutoReport(pAosRequest);
    }


    @Override
    public void initService() {
        msgPushAdapter.initService();
        msgPushAdapter.registerCallBack("MsgPushPackage", this);
    }

    public synchronized void registerCallBack(String key, MsgPushCallBack callback) {
        if (callback != null && !callBacks.contains(callback)) {
            callBacks.put(key, callback);
        }
    }

    @Override
    public void notifyAutoPushMessage(MsgPushInfo msg) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.notifyAutoPushMessage(msg);
        }
    }

    @Override
    public void notifyAimPoiPushMessage(MsgPushInfo msg) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.notifyAimPoiPushMessage(msg);
        }
    }

    @Override
    public void notifyAimRoutePushMessage(RouteMsgPushInfo routeMsgPushInfo) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.notifyAimRoutePushMessage(routeMsgPushInfo);
        }
    }

    @Override
    public void notifyMobileLinkPushMessage(MsgPushInfo msg) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.notifyMobileLinkPushMessage(msg);
        }
    }

    @Override
    public void onRecvAckGSendToPhoneResponse(MsgPushResponseInfo gSendToPhoneResponseParam) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.onRecvAckGSendToPhoneResponse(gSendToPhoneResponseParam);
        }
    }

    @Override
    public void onRecvAckGWsTserviceInternalLinkAutoReportResponse(MsgPushResponseInfo gWsTserviceInternalLinkAutoReportResponseParam) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.onRecvAckGWsTserviceInternalLinkAutoReportResponse(gWsTserviceInternalLinkAutoReportResponseParam);
        }
    }

    private static final class SInstanceHolder {
        static final MsgPushPackage sInstance = new MsgPushPackage();
    }
}
