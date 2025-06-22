package com.sgm.navi.mapservice.callback;

public interface OnLocationChangeListener {

    /**
     * 当前位置信息改变回调.
     *
     * @param locationInfo String, BaseLocationInfo对应的json类型字符串.
     */
    void onLocationChange(String locationInfo);
}
