package com.fy.navi.service.logicpaket.user.forecast;

import com.fy.navi.service.adapter.user.forecast.ForecastAdapter;
import com.fy.navi.service.adapter.user.forecast.ForecastAdapterCallback;
import com.fy.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;

import java.util.ArrayList;
import java.util.List;


public class ForecastPackage implements ForecastAdapterCallback {
    private final ForecastAdapter mForecastAdapter;
    private final List<ForecastCallBack> mCallBacks = new ArrayList<>();

    public ForecastPackage() {
        mForecastAdapter = ForecastAdapter.getInstance();
    }

    @Override
    public void initService() {
        mForecastAdapter.initService();
        mForecastAdapter.registerCallBack("ForecastPackage", this);
    }

    public static ForecastPackage getInstance() {
        return SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final ForecastPackage INSTANCE = new ForecastPackage();
    }

    /**
     * 注册回调
     * @param callBack 回调
     */
    public void registerCallBack(final ForecastCallBack callBack) {
        mCallBacks.add(callBack);
    }


    /**
     * 异步获取在线预测常去目的地(包含家、公司预测)
     * @param param 请求参数
     */
    public void getOnlineForecastArrivedData(final ForecastArrivedDataInfo param) {
        mForecastAdapter.getOnlineForecastArrivedData(param);
    }

    /**
     * 根据POI名称置顶
     * @param name 常去地点名称
     */
    public void topArrivedData(final String name) {
        mForecastAdapter.topArrivedData(name);
    }

    /**
     * 根据POI名称删除常去地点
     * @param name 常去地点名称
     */
    public void deleteLocalArrivedData(final String name) {
        mForecastAdapter.deleteLocalArrivedData(name);
    }

    /**
     * 获取常去地点信息列表
     * @return 返回数据
     */
    public ArrayList<OftenArrivedItemInfo> getArrivedDataList() {
        return mForecastAdapter.getArrivedDataList();
    }

    /**
     * 添加常去地点
     * @param info 添加的常去地点
     * @return 返回结果
     */
    public int addLocalArrivedData(final OftenArrivedItemInfo info) {
        return mForecastAdapter.addLocalArrivedData(info);
    }

    @Override
    public void onInit(final int result) {
        for (ForecastCallBack callBack : mCallBacks) {
            callBack.onInit(result);
        }
    }

    @Override
    public void onSetLoginInfo(final int result) {
        for (ForecastCallBack callBack : mCallBacks) {
            callBack.onSetLoginInfo(result);
        }
    }

    @Override
    public void onForecastArrivedData(final ForecastArrivedDataInfo data) {
        for (ForecastCallBack callBack : mCallBacks) {
            callBack.onForecastArrivedData(data);
        }
    }
}
