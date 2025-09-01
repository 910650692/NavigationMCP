package com.sgm.navi.service.logicpaket.user.msgpush;

import com.android.utils.log.Logger;
import com.sgm.navi.service.adapter.user.msgpush.MsgPushAdapter;
import com.sgm.navi.service.adapter.user.msgpush.MsgPushAdapterCallback;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.user.msgpush.MsgCarInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;

import java.util.ArrayList;
import java.util.Hashtable;


public class MsgPushPackage implements MsgPushAdapterCallback{
    private final MsgPushAdapter msgPushAdapter;
    private final Hashtable<String, MsgPushCallBack> callBacks;
    private long mLastSendTime;

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
        long result;
        if (mLastSendTime + (10 * 60 * 1000) < System.currentTimeMillis()) {
            result = msgPushAdapter.sendReqSendToPhone(pAosRequest);
            mLastSendTime = System.currentTimeMillis();
        } else {
            result = -1L;
        }
        Logger.d("sendReqSendToPhone: result = ", result);
        return result;
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

    public long sendReqWsTserviceInternalLinkAutoReport(MsgCarInfo msgCarInfo) {
        return msgPushAdapter.sendReqWsTserviceInternalLinkAutoReport(msgCarInfo);
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

    public synchronized void unregisterCallBack(String key) {
        callBacks.remove(key);
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
    public void notifyPlanPrefPushMessage(ArrayList<Integer> planPrefs) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.notifyPlanPrefPushMessage(planPrefs);
        }
    }

    @Override
    public void notifyDestinationPushMessage(RouteMsgPushInfo routeMsgPushInfo) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.notifyDestinationPushMessage(routeMsgPushInfo);
        }
    }

    @Override
    public void notifyQuitNaviPushMessage() {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.notifyQuitNaviPushMessage();
        }
    }

    @Override
    public void notifyMobileLinkPushMessage(MsgPushInfo msg) {
        for (MsgPushCallBack callBack : callBacks.values()) {
            callBack.notifyMobileLinkPushMessage(msg);
        }
    }

    /**
     * 手机端点击断开连接消息
     *
     * @param status 1：互联 0：断开
     */
    @Override
    public void notifyDisconnectFromMobileMessage(int status) {
        if (status == 1) AccountPackage.getInstance().accountLogoutRequest();
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
