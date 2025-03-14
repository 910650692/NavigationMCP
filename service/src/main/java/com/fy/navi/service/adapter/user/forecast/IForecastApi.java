package com.fy.navi.service.adapter.user.forecast;

import com.fy.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;

import java.util.ArrayList;

public interface IForecastApi {

    // 注册回调
    void registerCallBack(String key, ForecastAdapterCallback callBack);

    // 初始化服务
    void initService();

    //添加常去地点
    int addLocalArrivedData(OftenArrivedItemInfo oftenArrivedItemInfo);

    //根据POI名称删除常去地点
    int deleteLocalArrivedData(String name);

    //获取常去地点信息Top列表
    ArrayList<OftenArrivedItemInfo> getArrivedDataList();

    //根据POI名称置顶
    int topArrivedData(String name);

    // 异步获取在线预测常去目的地(包含家、公司预测)
    int getOnlineForecastArrivedData(ForecastArrivedDataInfo param);

}
