package com.fy.navi.service.define.code;

public class UserDataCode {
    public static final int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;

    public static final int ForecastLocal = 0;
    public static final int FavoriteLocal = 1;

    //判断APP是否为首次启动
    public static final String SETTING_FIRST_LAUNCH = "isFirstLaunch";
    //获取上一次定位信息
    public static final String SETTING_LAST_LOCATION = "location";
    //获取用户登录信息
    public static final String SETTING_GET_USERINFO = "userinfo";

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

    /** SyncEventType 属性值 **/
    public static final int SyncSdkEventBackupStart = 1; // 用户处于退出登录状态，开始同步事件
    public static final int SyncSdkEventBackupEnd = 2; // 用户处于退出登录状态，完成同步事件
    public  static final int SyncSdkEventSyncStart = 3; // 用户处于登录状态，开始同步事件
    public static final int SyncSdkEventSyncEnd = 4; // 用户处于登录状态，完成同步事件
    public static final int	SyncSdkEventDataUpdated = 5; // 同步期间数据发生变化事件
    public static final int SyncSdkEventWantMergeLocalDataToUser = 6; // 用户处于登录状态，本地Local数据合并到当前用户数据事件
    public static final int SyncSdkEventMergeStart = 7; // 合并开始事件
    public static final int SyncSdkEventMergeEnd = 8; // 合并完成事件
    public static final int SyncSdkEventFirstSync = 9; // 用于处于登录状态，从服务端下发的数据第一次发生变化的事件
    public static final int SyncSdkEventDatabaseInitDone = 10; // 数据库初始化(包括更新完成)的事件

}
