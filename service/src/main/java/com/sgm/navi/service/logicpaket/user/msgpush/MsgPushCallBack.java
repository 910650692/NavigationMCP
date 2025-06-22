package com.sgm.navi.service.logicpaket.user.msgpush;

import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushInfo;

public interface MsgPushCallBack {

    default void notifyAutoPushMessage(MsgPushInfo msg) {
    }

    default void notifyAimPoiPushMessage(MsgPushInfo msg) {
    }

    default void notifyAimRoutePushMessage(RouteMsgPushInfo routeMsgPushInfo) {
    }

    default void notifyMobileLinkPushMessage(MsgPushInfo msg) {
    }

    default void onRecvAckGSendToPhoneResponse(MsgPushResponseInfo gSendToPhoneResponseParam) {
    }

    default void onRecvAckGWsTserviceInternalLinkAutoReportResponse(MsgPushResponseInfo gWsTserviceInternalLinkAutoReportResponseParam) {
    }
}
