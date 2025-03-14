package com.fy.navi.service.adapter.user.carconnect;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.user.carconnect.CarConnectRequestBaseBean;
import com.fy.navi.service.define.user.carconnect.TaskResultBean;

public interface ICarConnectApi {

    void registerCallBack(String key, CarConnectAdapterCallback callBack);

    void initService();

    /**
     * 可连接设备请求
     * @param geoPoint
     * @return
     */
    TaskResultBean requestMobileLinkable(GeoPoint geoPoint);

    /**
     * 连接设备请求
     * @param deviceId 设备编号
     * @return
     */
    TaskResultBean requestMobileLink(long deviceId);

    /**
     * 终止所有请求
     */
    void abort();

    /**
     * 终止指定任务额请求
     * @param taskId
     */
    void abort(long taskId);

    /**
     * 执行一个网络请求,车机互联的通用上报接口(除去导航上报)
     * @param pAosRequest
     * @return
     */
    long sendReqWsTserviceInternalLinkAutoReport(CarConnectRequestBaseBean pAosRequest);
}
