package com.sgm.navi.service.adapter.user.msgpush;

import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushResponseInfo;

public interface MsgPushAdapterCallback {

    /**
     * initService
     */
    void initService();

    /**
     * 运营推送消息通知
     * @param msg
     */
    void notifyAutoPushMessage(MsgPushInfo msg);

    /**
     * AIMPOI(send2car)推送消息通知
     * @param msg
     */
    void notifyAimPoiPushMessage(MsgPushInfo msg);

    /**
     * 手机发送路线推送消息通知
      * @param routeMsgPushInfo
     */
    void notifyAimRoutePushMessage(RouteMsgPushInfo routeMsgPushInfo);

    /**
     * 手机端推送发现可连接车机消息
     * @param msg
     */
    void notifyMobileLinkPushMessage(MsgPushInfo msg);

    default void notifyDisconnectFromMobileMessage(int status) {

    }

    /**
     * 网络库线程中回调业务应答类
     * @param sendToPhoneResponseParam
     */
    void onRecvAckGSendToPhoneResponse(MsgPushResponseInfo sendToPhoneResponseParam);

    /**
     * 网络库线程中回调业务应答类
      * @param gwsTserviceInternalLinkAutoReportResponseParam
     */
    void onRecvAckGWsTserviceInternalLinkAutoReportResponse(MsgPushResponseInfo gwsTserviceInternalLinkAutoReportResponseParam);
}
