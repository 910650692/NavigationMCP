package com.fy.navi.service.logicpaket.user.forecast;

import com.fy.navi.service.define.user.forecast.ForecastArrivedDataInfo;

public interface ForecastCallBack {

    //异步初始化回调
    void onInit(int result);
    // 切换账号加载数据回调
    void onSetLoginInfo(int result);
    // 异步获取在线预测常去地点(包含家、公司数据)
    void onForecastArrivedData(ForecastArrivedDataInfo data);
}
