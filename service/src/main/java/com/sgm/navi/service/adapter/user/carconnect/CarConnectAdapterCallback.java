package com.sgm.navi.service.adapter.user.carconnect;

import com.sgm.navi.service.define.user.carconnect.CarConnectResponseBaseBean;
import com.sgm.navi.service.define.user.carconnect.MobileLinkableResultBean;
import com.sgm.navi.service.define.user.carconnect.TaskResultBean;
import com.sgm.navi.service.define.user.group.MsgPushItemBean;

public interface CarConnectAdapterCallback {

    /**
     * initService
     */
    void initService();

    /**
     * 手机端推送发现可连接车机消息
     * @param msg
     */
    void notifyMobileLinkPushMessage(MsgPushItemBean msg);

    /**
     * 网络库线程中回调业务应答类
     * @param responseBaseBean
     */
    void onRecvAckGWsTserviceInternalLinkAutoReportResponse(CarConnectResponseBaseBean responseBaseBean);

    /**
     * 可连接设备的结果回调
     * @param result
     */
    void onMobileLinkableResult(MobileLinkableResultBean result);

    /**
     * 连接设备的结果回调
     * @param result
     */
    void onMobileLinkResult(TaskResultBean result);
}
