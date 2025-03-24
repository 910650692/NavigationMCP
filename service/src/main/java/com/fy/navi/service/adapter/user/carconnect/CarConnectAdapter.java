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

    /**
     * initService
     */
    public void initService() {
        mCarConnectApi.initService();
    }

    /**
     * registerCallBack
     * @param key
     * @param callBack
     */
    public void registerCallBack(final String key, final CarConnectAdapterCallback callBack) {
        mCarConnectApi.registerCallBack(key, callBack);
    }

    /**
     * requestMobileLinkable
     * @param geoPoint
     * @return TaskResultBean
     */
    public TaskResultBean requestMobileLinkable(final GeoPoint geoPoint) {
        return mCarConnectApi.requestMobileLinkable(geoPoint);
    }

    /**
     * requestMobileLink
     * @param deviceId
     * @return TaskResultBean
     */
    public TaskResultBean requestMobileLink(final long deviceId) {
        return mCarConnectApi.requestMobileLink(deviceId);
    }

    /**
     * abort
     */
    public void abort() {
        mCarConnectApi.abort();
    }

    /**
     * abort
     * @param taskId
     */
    public void abort(final long taskId) {
        mCarConnectApi.abort(taskId);
    }

    /**
     * sendReqWsTserviceInternalLinkAutoReport
     * @param aosRequest
     * @return long
     */
    public long sendReqWsTserviceInternalLinkAutoReport(final CarConnectRequestBaseBean aosRequest) {
        return mCarConnectApi.sendReqWsTserviceInternalLinkAutoReport(aosRequest);
    }

    public static CarConnectAdapter getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static final class SingletonHolder {
        private static final CarConnectAdapter INSTANCE = new CarConnectAdapter();
    }

}
