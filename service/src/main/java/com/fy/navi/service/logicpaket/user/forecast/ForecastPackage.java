package com.fy.navi.service.logicpaket.user.forecast;

import com.fy.navi.service.adapter.user.forecast.ForecastAdapter;
import com.fy.navi.service.adapter.user.forecast.ForecastAdapterCallback;
import com.fy.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;

import java.util.ArrayList;
import java.util.List;

public class ForecastPackage implements ForecastAdapterCallback {
    private final ForecastAdapter forecastAdapter;
    private final List<ForecastCallBack> callBacks = new ArrayList<>();

    public ForecastPackage() {
        forecastAdapter = ForecastAdapter.getInstance();
    }

    @Override
    public void initService() {
        forecastAdapter.initService();
        forecastAdapter.registerCallBack("ForecastPackage", this);
    }

    public static ForecastPackage getInstance() {
        return SInstanceHolder.sInstance;
    }

    private static final class SInstanceHolder {
        static final ForecastPackage sInstance = new ForecastPackage();
    }

    public void registerCallBack(ForecastCallBack callBack) {
        callBacks.add(callBack);
    }

    public void getOnlineForecastArrivedData(ForecastArrivedDataInfo param) {
        forecastAdapter.getOnlineForecastArrivedData(param);
    }

    /**
     * 根据POI名称置顶
     * @param name 常去地点名称
     */
    public void topArrivedData(String name) {
        forecastAdapter.topArrivedData(name);
    }

    /**
     * 根据POI名称删除常去地点
     * @param name 常去地点名称
     */
    public void deleteLocalArrivedData(String name) {
        forecastAdapter.deleteLocalArrivedData(name);
    }

    /**
     * 获取常去地点信息列表
     * @return 返回数据
     */
    public ArrayList<OftenArrivedItemInfo> getArrivedDataList() {
        return forecastAdapter.getArrivedDataList();
    }

    /**
     * 添加常去地点
     * @param info 添加的常去地点
     * @return
     */
    public int addLocalArrivedData(OftenArrivedItemInfo info) {
        return forecastAdapter.addLocalArrivedData(info);
    }

    @Override
    public void onInit(int result) {
        for (ForecastCallBack callBack : callBacks) {
            callBack.onInit(result);
        }
    }

    @Override
    public void onSetLoginInfo(int result) {
        for (ForecastCallBack callBack : callBacks) {
            callBack.onSetLoginInfo(result);
        }
    }

    @Override
    public void onForecastArrivedData(ForecastArrivedDataInfo data) {
        for (ForecastCallBack callBack : callBacks) {
            callBack.onForecastArrivedData(data);
        }
    }
}
