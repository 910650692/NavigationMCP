package com.sgm.navi.service.define.user.msgpush;

public class MsgCarInfo {
    public NaviLocInfo naviLocInfo = new NaviLocInfo(); //坐标信息，包含纬度 (lat) 和经度 (lon)。
    public String plateNum = ""; //车牌号
    public int parkStatus; //停车状态，1 表示停车，0 表示非停车。

    public class NaviLocInfo {
        public double lon; //经度
        public double lat; //纬度
    }
}