package com.fy.navi.service.adapter.user.forecast;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;

import java.util.ArrayList;
import java.util.Objects;

public final class ForecastAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(ForecastAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "ForecastAdapterImpl";
    private IForecastApi mForecastApi;

    private ForecastAdapter() {
        mForecastApi = (IForecastApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    /**
     * 初始化服务
     */
    public void initService() {
        mForecastApi.initService();
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param callBack 回调
     */
    public void registerCallBack(final String key, final ForecastAdapterCallback callBack) {
        mForecastApi.registerCallBack(key, callBack);
    }

    /**
     * 添加常去地点
     * @param oftenArrivedItemInfo 添加的常去地点
     * @return 返回结果
     */
    public int addLocalArrivedData(final OftenArrivedItemInfo oftenArrivedItemInfo) {
        return mForecastApi.addLocalArrivedData(oftenArrivedItemInfo);
    }

    /**
     * 根据POI名称删除常去地点
     * @param name 常去地点名称
     */
    public void deleteLocalArrivedData(final String name) {
        mForecastApi.deleteLocalArrivedData(name);
    }

    /**
     * 获取常去地点信息列表
     * @return 返回数据
     */
    public ArrayList<OftenArrivedItemInfo> getArrivedDataList() {
        return mForecastApi.getArrivedDataList();
    }

    /**
     * 根据POI名称置顶
     * @param name 常去地点名称
     * @return 返回错误码
     */
    public int topArrivedData(final String name) {
        return mForecastApi.topArrivedData(name);
    }

    /**
     * 异步获取在线预测常去目的地(包含家、公司预测)
     * @param param 请求参数
     * @return 请求结果
     */
    public int getOnlineForecastArrivedData(final ForecastArrivedDataInfo param) {
        return mForecastApi.getOnlineForecastArrivedData(param);
    }

    public static ForecastAdapter getInstance() {
        return ForecastAdapterHolder.INSTANCE;
    }

    private final static class ForecastAdapterHolder {
        private static final ForecastAdapter INSTANCE = new ForecastAdapter();
    }
}
