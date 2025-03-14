package com.fy.navi.service.adapter.user.carconnect;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.user.carconnect.CarConnectRequestBaseBean;
import com.fy.navi.service.define.user.carconnect.TaskResultBean;

import java.util.Objects;

public class CarConnectAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(CarConnectAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "CarConnectAdapterImpl";
    private ICarConnectApi mCarConnectApi;

    public CarConnectAdapter() {
        mCarConnectApi = (ICarConnectApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initService() {
        mCarConnectApi.initService();
    }

    public void registerCallBack(String key, CarConnectAdapterCallback callBack) {
        mCarConnectApi.registerCallBack(key, callBack);
    }

    public TaskResultBean requestMobileLinkable(GeoPoint geoPoint) {
        return mCarConnectApi.requestMobileLinkable(geoPoint);
    }

    public TaskResultBean requestMobileLink(long deviceId) {
        return mCarConnectApi.requestMobileLink(deviceId);
    }

    public void abort() {
        mCarConnectApi.abort();
    }

    public void abort(long taskId) {
        mCarConnectApi.abort(taskId);
    }

    public long sendReqWsTserviceInternalLinkAutoReport(CarConnectRequestBaseBean pAosRequest) {
        return mCarConnectApi.sendReqWsTserviceInternalLinkAutoReport(pAosRequest);
    }

    public static CarConnectAdapter getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static final class SingletonHolder {
        private static final CarConnectAdapter INSTANCE = new CarConnectAdapter();
    }

}
