package com.sgm.navi.service.define.code;

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

    //判断数据库里是否有离线数据 有数据返回“1”，没有返回“0”或空
    public static final String SETTING_DOWNLOAD_LIST = "setting_download_list";

    //uuid
    public static final String SETTING_ACCOUNT_UUID = "setting_account_uuid";

    //app key
    public static final String SETTING_ACCOUNT_APP_KEY = "setting_account_app_key";

    //已经下载城市
    public static final String MAP_DATA_DOWNLOADED_CITY_LIST = "map_data_downloaded_city_list";

    //nd是否显示回家view
    public static final String MAP_ND_GO_HOME_KEY = "nd_go_home_key";

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

    // 离线地图数据操作状态
    public static final int OPERATION_TYPE_START = 0; // 下载
    public static final int OPERATION_TYPE_PAUSE = 1; // 暂停
    public static final int OPERATION_TYPE_CANCEL = 2; // 取消
    public static final int OPERATION_TYPE_DELETE = 3; // 删除
    public static final int OPERATION_TYPE_MAX = 4; //

    //正常
    public static final int OPT_DONE = 0x2000000;
    //无网络连接
    public static final int OPT_NET_DISCONNECT = OPT_DONE + 1;
    // 下载网络异常
    public static final int OPT_DOWNLOAD_NET_ERROR = OPT_NET_DISCONNECT + 1;
    //剩余空间小于阈值
    public static final int OPT_NO_SPACE_LEFTED = 10;
    // 存储空间可能不足，是否继续下载？
    public static final int OPT_SPACE_NOT_ENOUGHT = OPT_NO_SPACE_LEFTED + 1;
    //操作异常
    public static final int OPT_ERROR = OPT_SPACE_NOT_ENOUGHT + 1;

    /** 收藏点类型 属性值  1家，2公司，3常去地址，0普通收藏点  **/
    public static final int FAVORITE_TYPE_POI = 0;
    public static final int FAVORITE_TYPE_HOME = 1;
    public static final int FAVORITE_TYPE_COMPANY = 2;
    public static final int FAVORITE_TYPE_ADDRESS = 3;

    /** SyncEventType 属性值 **/
    public static final int SYNC_SDK_EVENT_BACK_UP_START = 1; // 用户处于退出登录状态，开始同步事件
    public static final int SYNC_SDK_EVENT_BACK_UP_END = 2; // 用户处于退出登录状态，完成同步事件
    public static final int SYNC_SDK_EVENT_SYNC_START = 3; // 用户处于登录状态，开始同步事件
    public static final int SYNC_SDK_EVENT_SYNCEND = 4; // 用户处于登录状态，完成同步事件
    public static final int SYNC_SDK_EVENT_DATA_UPDATED = 5; // 同步期间数据发生变化事件
    public static final int SYNC_SDK_EVENT_WANT_MERGE_LOCAL_DATA_TO_USER = 6; // 用户处于登录状态，本地Local数据合并到当前用户数据事件
    public static final int SYNC_SDK_EVENT_MERGE_START = 7; // 合并开始事件
    public static final int SYNC_SDK_EVENT_MERGE_END = 8; // 合并完成事件
    public static final int SYNC_SDK_EVENT_FIRST_SYNC = 9; // 用于处于登录状态，从服务端下发的数据第一次发生变化的事件
    public static final int SYNC_SDK_EVENT_DATA_BASE_INIT_DONE = 10; // 数据库初始化(包括更新完成)的事件
    public static final String GUIDE_LOGIN_LAST_TIME = "GUIDE_LOGIN_LAST_TIME";
}
