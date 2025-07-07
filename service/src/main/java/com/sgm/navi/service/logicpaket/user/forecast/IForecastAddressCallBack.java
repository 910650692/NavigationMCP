package com.sgm.navi.service.logicpaket.user.forecast;

import com.sgm.navi.service.define.user.forecast.OftenArrivedItemInfo;

public interface IForecastAddressCallBack {

    void AddForecastInfo(int type, OftenArrivedItemInfo oftenArrivedItemInfo);

    void addressClick(int type);

}
