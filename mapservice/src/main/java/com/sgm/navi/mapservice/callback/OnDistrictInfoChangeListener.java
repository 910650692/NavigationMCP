package com.sgm.navi.mapservice.callback;

public interface OnDistrictInfoChangeListener {

    /**
     * 行政区域信息变化回调.
     * @param districtInfo String, BaseDistrictInfo对应的json类型字符串.
     */
    void onDistrictInfoChange(String districtInfo);
}
