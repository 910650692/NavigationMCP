package com.fy.navi.service.logicpaket.user.carconnect;

import com.fy.navi.service.adapter.user.carconnect.CarConnectAdapter;
import com.fy.navi.service.adapter.user.carconnect.CarConnectAdapterCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.user.carconnect.CarConnectRequestBaseBean;
import com.fy.navi.service.define.user.carconnect.CarConnectResponseBaseBean;
import com.fy.navi.service.define.user.carconnect.MobileLinkableResultBean;
import com.fy.navi.service.define.user.carconnect.TaskResultBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;

import java.util.ArrayList;
import java.util.List;

public class CarConnectPackage implements CarConnectAdapterCallback {
    private final CarConnectAdapter carConnectAdapter;
    private final List<CarConnectCallBack> callBacks = new ArrayList<>();


    public CarConnectPackage() {
        carConnectAdapter = CarConnectAdapter.getInstance();
    }
    @Override
    public void initService() {
        carConnectAdapter.initService();
        carConnectAdapter.registerCallBack("CarConnectPackage", this);
    }

    public synchronized void registerCallBack(CarConnectCallBack callback) {
        if (callback != null && !callBacks.contains(callback)) {
            callBacks.add(callback);
        }
    }

    public static CarConnectPackage getInstance() {
        return SInstanceHolder.sInstance;
    }

    private static final class SInstanceHolder {
        static final CarConnectPackage sInstance = new CarConnectPackage();
    }

    public TaskResultBean requestMobileLinkable(GeoPoint geoPoint) {
        return carConnectAdapter.requestMobileLinkable(geoPoint);
    }

    public TaskResultBean requestMobileLink(long deviceId) {
        return carConnectAdapter.requestMobileLink(deviceId);
    }

    public long sendReqWsTserviceInternalLinkAutoReport(CarConnectRequestBaseBean pAosRequest) {
        return carConnectAdapter.sendReqWsTserviceInternalLinkAutoReport(pAosRequest);
    }

    public void abort() {
        carConnectAdapter.abort();
    }

    public void abort(long taskId) {
        carConnectAdapter.abort(taskId);
    }


    @Override
    public void notifyMobileLinkPushMessage(MsgPushItemBean msg) {
        for (CarConnectCallBack callBack : callBacks) {
            callBack.notifyMobileLinkPushMessage(msg);
        }
    }

    @Override
    public void onRecvAckGWsTserviceInternalLinkAutoReportResponse(CarConnectResponseBaseBean responseBaseBean) {
        for (CarConnectCallBack callBack : callBacks) {
            callBack.onRecvAckGWsTserviceInternalLinkAutoReportResponse(responseBaseBean);
        }
    }

    @Override
    public void onMobileLinkableResult(MobileLinkableResultBean result) {
        for (CarConnectCallBack callBack : callBacks) {
            callBack.onMobileLinkableResult(result);
        }
    }

    @Override
    public void onMobileLinkResult(TaskResultBean result) {
        for (CarConnectCallBack callBack : callBacks) {
            callBack.onMobileLinkResult(result);
        }
    }
}