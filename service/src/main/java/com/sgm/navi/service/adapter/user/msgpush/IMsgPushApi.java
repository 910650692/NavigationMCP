package com.sgm.navi.service.adapter.user.msgpush;


import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.user.msgpush.MsgCarInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushInfo;

import java.util.ArrayList;

public interface IMsgPushApi {

    /**
     * registerCallBack
     * @param key
     * @param callBack
     */
    void registerCallBack(String key, MsgPushAdapterCallback callBack);

    /**
     * initService
     */
    void initService();

    /**
     * 开启消息推送监听
     * @param userId 用户ID
     */
    void startListen(String userId);

    /**
     * 终止消息推送监听
     */
    void stopListen();

    /**
     * 获取本地历史运营推送消息
     * @return list
     */
    ArrayList<MsgPushInfo> getAutoPushMessages();

    /**
     * 获取本地历史AIMPOI(send2car)推送消息
     * @return list
     */
    ArrayList<MsgPushInfo> getAimPoiPushMessages();

    /**
     * 获取本地历史中手机发送的路线推送消息
     * @return list
     */
    ArrayList<MsgPushInfo> getAimRoutePushMessages();

    /**
     * 修改手机推送的路线终点POI的名称
     * @param msgId
     * @param msgName
     */
    void updateAimRouteEndPoiName(long msgId, String msgName);

    /**
     * 执行一个网络请求,send2phone服务接口
     * @param aosRequest
     * @return sendReqSendToPhone
     */
    long sendReqSendToPhone(MsgPushRequestInfo aosRequest);

    /**
     * 连接设备请求
     * @param deviceId
     * @param userLocation
     * @return MsgPushResponseInfo
     */
    MsgPushResponseInfo request(long deviceId, GeoPoint userLocation);

    /**
     * 终止所有请求
     */
    void abort();

    /**
     * 终止指定任务额请求
     * @param taskId 任务ID
     */
    void abort(long taskId);

    /**
     * 执行一个网络请求,车机互联的通用上报接口(除去导航上报)
     * @param msgCarInfo
     * @return long
     */
    long sendReqWsTserviceInternalLinkAutoReport(MsgCarInfo msgCarInfo);

}
