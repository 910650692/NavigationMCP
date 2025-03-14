package com.fy.navi.service.adapter.user.forecast;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;

import java.util.ArrayList;
import java.util.Objects;

public class ForecastAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(ForecastAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "ForecastAdapterImpl";
    private IForecastApi mForecastApi;

    private ForecastAdapter() {
        mForecastApi = (IForecastApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initService() {
        mForecastApi.initService();
    }

    public void registerCallBack(String key, ForecastAdapterCallback callBack) {
        mForecastApi.registerCallBack(key, callBack);
    }

    public int addLocalArrivedData(OftenArrivedItemInfo oftenArrivedItemInfo) {
        return mForecastApi.addLocalArrivedData(oftenArrivedItemInfo);
    }

    public void deleteLocalArrivedData(String name) {
        mForecastApi.deleteLocalArrivedData(name);
    }

    public ArrayList<OftenArrivedItemInfo> getArrivedDataList() {
        return mForecastApi.getArrivedDataList();
    }

    public int topArrivedData(String name) {
        return mForecastApi.topArrivedData(name);
    }

    public int getOnlineForecastArrivedData(ForecastArrivedDataInfo param) {
        return mForecastApi.getOnlineForecastArrivedData(param);
    }

    public static ForecastAdapter getInstance() {
        return ForecastAdapterHolder.INSTANCE;
    }

    private static class ForecastAdapterHolder {
        private static final ForecastAdapter INSTANCE = new ForecastAdapter();
    }
}
