package com.fy.navi.service.define.setting;

public interface SettingConstant {
    // TODO: 2024/12/22 这个导航类型由客户端决定，不予存储，后续需要删除
    boolean ISSIMULATEMODE = true;
    /*** 是否深色模式 **/
    boolean ISNIGHTMODE = false;


    /***北京天安门 **/
    double DEFAULT_LAT_BJ = 39.908696;
    double DEFAULT_LON_BJ = 116.397496;
    double DEFAULT_ALT_BJ = 0.0;
    /*** 上海东方明珠 **/
     double DEFAULT_LAT_SH = 31.239668;
     double DEFAULT_LON_SH = 121.499779;
     double DEFAULT_ALT_SH = 0.0;
}
