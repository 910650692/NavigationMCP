package com.fy.navi.service.define.code;

public final class UserDataCode {

    private UserDataCode() {

    }

    //判断APP是否为首次启动
    public static final String SETTING_FIRST_LAUNCH = "isFirstLaunch";
    //获取上一次定位信息
    public static final String SETTING_LAST_LOCATION = "location";
    //获取用户登录信息
    public static final String SETTING_GET_USERINFO = "userinfo";
    //是否首次下载“基础导航数据包”
    public static final String SETTING_DOWNLOAD_COUNTRY = "countryMapData";

    //消息说明的时间   一天只显示一次
    public static final String SETTING_MESSAGE_LIMIT_TIME = "limittime";
    //离线地图更新消息提示 （每15日 check一次）
    public static final String SETTING_MESSAGE_CHECK_MAP_DATA_TIME = "checktime";

    //消息中心15天检测一次
    public static final String SETTING_MESSAGE_MAP_CHECK = "mapcheckupdate";
    //消息中心45天无网检测一次
    public static final String SETTING_MESSAGE_NET_LESS = "mapnetless";

    // 离线地图数据下载状态
    public static final int TASK_STATUS_CODE_READY = 0;
    public static final int TASK_STATUS_CODE_WAITING = 1; //
    public static final int TASK_STATUS_CODE_DOING = 2;
    public static final int TASK_STATUS_CODE_PAUSE = 3;
    public static final int TASK_STATUS_CODE_DONE = 4;
    public static final int TASK_STATUS_CODE_CHECKING = 5;
    public static final int TASK_STATUS_CODE_CHECKED = 6;
    public static final int TASK_STATUS_CODE_UNZIPPING = 7;
    public static final int TASK_STATUS_CODE_UNZIPPED = 8;
    public static final int TASK_STATUS_CODE_SUCCESS = 9; // 已下载
    public static final int TASK_STATUS_CODE_ERR = 10;
    public static final int TASK_STATUS_CODE_RECOVERING = 11;
    public static final int TASK_STATUS_CODE_INVALID = 12;
    public static final int TASK_STATUS_CODE_MAX = 13;

}
