package com.sgm.navi.service.logicpaket.user.carconnect;

import com.sgm.navi.service.adapter.user.carconnect.CarConnectAdapter;
import com.sgm.navi.service.adapter.user.carconnect.CarConnectAdapterCallback;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.user.carconnect.CarConnectRequestBaseBean;
import com.sgm.navi.service.define.user.carconnect.CarConnectResponseBaseBean;
import com.sgm.navi.service.define.user.carconnect.MobileLinkableResultBean;
import com.sgm.navi.service.define.user.carconnect.TaskResultBean;
import com.sgm.navi.service.define.user.group.MsgPushItemBean;

import java.util.ArrayList;
import java.util.List;

public class CarConnectPackage implements CarConnectAdapterCallback {
    private final CarConnectAdapter mCarConnectAdapter;
    private final List<CarConnectCallBack> mCallBacks = new ArrayList<>();


    public CarConnectPackage() {
        mCarConnectAdapter = CarConnectAdapter.getInstance();
    }
    @Override
    public void initService() {
        mCarConnectAdapter.initService();
        mCarConnectAdapter.registerCallBack("CarConnectPackage", this);
    }

    /**
     * 注册callback监听
     * @param callback
     */
    public synchronized void registerCallBack(final CarConnectCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.add(callback);
        }
    }

    public static CarConnectPackage getInstance() {
        return SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final CarConnectPackage INSTANCE = new CarConnectPackage();
    }

    /**
     * 可连接设备请求
     * @param geoPoint
     * @return TaskResultBean
     */
    public TaskResultBean requestMobileLinkable(final GeoPoint geoPoint) {
        return mCarConnectAdapter.requestMobileLinkable(geoPoint);
    }

    /**
     * requestMobileLink
     * @param deviceId
     * @return TaskResultBean
     */
    public TaskResultBean requestMobileLink(final long deviceId) {
        return mCarConnectAdapter.requestMobileLink(deviceId);
    }

    /**
     * sendReqWsTserviceInternalLinkAutoReport
     * @param aosRequest
     * @return long
     */
    public long sendReqWsTserviceInternalLinkAutoReport(final CarConnectRequestBaseBean aosRequest) {
        return mCarConnectAdapter.sendReqWsTserviceInternalLinkAutoReport(aosRequest);
    }

    /**
     * abort
     */
    public void abort() {
        mCarConnectAdapter.abort();
    }

    /**
     * abort
     * @param taskId
     */
    public void abort(final long taskId) {
        mCarConnectAdapter.abort(taskId);
    }

    @Override
    public void notifyMobileLinkPushMessage(final MsgPushItemBean msg) {
        for (CarConnectCallBack callBack : mCallBacks) {
            callBack.notifyMobileLinkPushMessage(msg);
        }
    }

    @Override
    public void onRecvAckGWsTserviceInternalLinkAutoReportResponse(final CarConnectResponseBaseBean responseBaseBean) {
        for (CarConnectCallBack callBack : mCallBacks) {
            callBack.onRecvAckGWsTserviceInternalLinkAutoReportResponse(responseBaseBean);
        }
    }

    @Override
    public void onMobileLinkableResult(final MobileLinkableResultBean result) {
        for (CarConnectCallBack callBack : mCallBacks) {
            callBack.onMobileLinkableResult(result);
        }
    }

    @Override
    public void onMobileLinkResult(final TaskResultBean result) {
        for (CarConnectCallBack callBack : mCallBacks) {
            callBack.onMobileLinkResult(result);
        }
    }
}