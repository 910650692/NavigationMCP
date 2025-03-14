package com.fy.navi.service.adapter.user.msgpush;

import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;

public interface MsgPushAdapterCallback {

    void initService();

    // 运营推送消息通知
    void notifyAutoPushMessage(MsgPushInfo msg);
    // AIMPOI(send2car)推送消息通知
    void notifyAimPoiPushMessage(MsgPushInfo msg);
    // 手机发送路线推送消息通知
    void notifyAimRoutePushMessage(RouteMsgPushInfo routeMsgPushInfo);
    // 手机端推送发现可连接车机消息
    void notifyMobileLinkPushMessage(MsgPushInfo msg);
    // 网络库线程中回调业务应答类
    void onRecvAckGSendToPhoneResponse(MsgPushResponseInfo gSendToPhoneResponseParam);
    // 网络库线程中回调业务应答类
    void onRecvAckGWsTserviceInternalLinkAutoReportResponse(MsgPushResponseInfo gWsTserviceInternalLinkAutoReportResponseParam);
}
