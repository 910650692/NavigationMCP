package com.android.utils.thread;

public enum LooperType {
    //底图专用线程
    MAIN_SCREEN_MAIN_MAP("MAIN_SCREEN_MAIN_MAP"),
    HUD_MAP("HUD_MAP"),
    CLUSTER_MAP("CLUSTER_MAP"),
    //nmea报文解析,定位专用线程
    GNSS("GNSS_HandlerThread"),
    GSV("GSV_HandlerThread"),
    //传感器 gyr,acc，传感器专用线程
    SENSOR("SENSOR_HandlerThread"),
    TEMPERATURE("TEMPERATURE_HandlerThread"),
    //高德定位回调专用线程
    LocInfoUpdate("LocInfoUpdate_HandlerThread"),
    /**
     * 频率不高的handler 线程
     */
    CommonBackGroundLooper("CommonBackGround_HandlerThread"),

    NaviSceneLooper("NaviScene_HandlerThread");

    LooperType(String threadName) {
        mThreadName = threadName;
    }

    public String getThreadName() {
        return mThreadName;
    }

    private String mThreadName;
}
