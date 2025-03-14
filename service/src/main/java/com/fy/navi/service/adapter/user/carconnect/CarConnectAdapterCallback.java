package com.fy.navi.service.adapter.user.carconnect;

import com.fy.navi.service.define.user.carconnect.CarConnectResponseBaseBean;
import com.fy.navi.service.define.user.carconnect.MobileLinkableResultBean;
import com.fy.navi.service.define.user.carconnect.TaskResultBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;

public interface CarConnectAdapterCallback {

    void initService();
    // 手机端推送发现可连接车机消息
    void notifyMobileLinkPushMessage(MsgPushItemBean msg);
    // 网络库线程中回调业务应答类
    void onRecvAckGWsTserviceInternalLinkAutoReportResponse(CarConnectResponseBaseBean responseBaseBean);
    // 可连接设备的结果回调
    void onMobileLinkableResult(MobileLinkableResultBean result);
    // 连接设备的结果回调
    void onMobileLinkResult(TaskResultBean result);
}
