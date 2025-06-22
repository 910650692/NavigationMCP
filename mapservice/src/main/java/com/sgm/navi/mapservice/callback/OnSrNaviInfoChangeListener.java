package com.sgm.navi.mapservice.callback;

public interface OnSrNaviInfoChangeListener {

    /**
     * 红绿灯倒计时信息.
     *
     * @param countDownLightInfo BaseLightCountdownInfo对应的json类型序列化字符串.
     */
    void onCountDownLightInfo(String countDownLightInfo);

    /**
     * 到达目的地.
     */
    void onNaviArrival();

    /**
     * 导航结束.
     */
    void onNaviStop();
}
