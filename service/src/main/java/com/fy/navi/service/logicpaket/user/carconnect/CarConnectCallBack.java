package com.fy.navi.service.logicpaket.user.carconnect;

import com.fy.navi.service.define.user.carconnect.CarConnectResponseBaseBean;
import com.fy.navi.service.define.user.carconnect.MobileLinkableResultBean;
import com.fy.navi.service.define.user.carconnect.TaskResultBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;

public interface CarConnectCallBack {

    // 手机端推送发现可连接车机消息
    default void notifyMobileLinkPushMessage(MsgPushItemBean msg){}
    // 网络库线程中回调业务应答类
    default void onRecvAckGWsTserviceInternalLinkAutoReportResponse(CarConnectResponseBaseBean responseBaseBean){}
    // 可连接设备的结果回调
    default void onMobileLinkableResult(MobileLinkableResultBean result){}
    // 连接设备的结果回调
    default void onMobileLinkResult(TaskResultBean result){}
}
