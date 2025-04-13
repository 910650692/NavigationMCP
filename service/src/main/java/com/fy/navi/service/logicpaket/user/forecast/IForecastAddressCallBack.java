package com.fy.navi.service.logicpaket.user.forecast;

import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;

public interface IForecastAddressCallBack {

    void AddForecastInfo(OftenArrivedItemInfo oftenArrivedItemInfo);

    void addressClick();

}
