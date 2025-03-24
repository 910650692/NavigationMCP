package com.fy.navi.service;

import java.io.File;

public interface GBLCacheFilePath extends AutoMapConstant {
    /*** AutoMap/libs/ **/
    public static final String DEBUG_LIBS_DIR = GBL_MAP + "libs" + File.separator;

    /*** 车道级回放文件所在目录 **/
    public static final String GPS_LANELOC_FOLDER = GBL_MAP + "loc_replay/";
    /*** 仿真回放路径 **/
    public static final String RECORDER_DATA_DIR = GBL_MAP + "recorder/";
    /*** 车道级导航 自车SR信息测试 **/
    public static final String OFFLINE_LANERESOURCE_DIR = GBL_MAP + "LaneCarSRResource/";

    /*** Auto Map log path **/
    public static final String BLS_LOG = GBL_MAP + "bllog";
    // TODO: 2025/2/28 定位测试用
//    public static final String BLS_LOG = FileUtils.APP_FILE_PATH + "bllog";

    public static final String BLS_COOKIES_PATH = GBL_MAP + "cookie";
    /*** AutoSDK资源文件的原始目录 **/
    public static final String BLS_ASSETS_PATH = "/android_assets/blRes/";
    /*** AutoSDK图层默认资源文件目录 **/
    public static final String BLS_ASSETS_LAYER_PATH = BLS_ASSETS_PATH + "LayerAsset/";
    /*** AutoSDK图层自定义资源文件目录 **/
    public static final String BLS_ASSETS_CUSTOM_LAYER_PATH = BLS_ASSETS_PATH + "CustomLayerAsset/";
    /*** 复制Assets文件根目录 **/
    public static final String COPY_ASSETS_DIR = GBL_MAP + "assets" + File.separator;
    /*** 用户数据目录，保存用户生成数据 **/
    public static final String USER_CACHE_PATH = GBL_MAP + "user" + File.separator;
    /*** 离线地图下载位置 **/
    public static final String OFFLINE_DOWNLOAD_DIR = GBL_MAP + "data/navi/compile_v2/chn" + File.separator;
    /*** 云端缓存地址 **/
    public static final String ONLINE_PATH = GBL_MAP + "online" + File.separator;
    /*** 配置车道级离线地图数据 **/
    public static final String LNDS_OFF_LINE_PATH = GBL_MAP + "data/navi/ld/chn";
    /*** 地图数据配置文件( all_city_compile.json 文件)所在目录 **/
    public static final String OFFLINE_CONF_PATH = COPY_ASSETS_DIR + "offline_conf/";
    /*** 账号数据所在目录 **/
    public static final String ACCOUNT_PATH = GBL_MAP + "account";
    /*** 云同步信息数据库文件( girf_sync.db 文件)所在目录 **/
    public static final String SYNC_PATH = GBL_MAP + "behavior";

    /***  预测数据库文件保存目录路径 **/
    public static final String FORECAST_PATH = GBL_MAP + "forecast";

    /*** 语音数据包配置文件 **/
    public static final String VOICE_CONF_PATH = GBL_MAP + "data/voice/voice_conf";
    /*** 讯飞语音数据包路径 **/
    public static final String VOICE_FLY_PATH = GBL_MAP + "assets/speech_conf";
    public static final String VOICE_MIT_PATH = GBL_MAP + "assets/speech_conf";
    /*** 标准语音数据包路径 **/
    public static final String DEFAULT_VOICE_PATH = GBL_MAP + "assets/speech_conf/xiaoyan.irf";

    public static final String MSG_FROM_PHONE_PATH = GBL_MAP + "data/msg";

    /*** 地图数据路径绝对地址 **/
    public static final String MAP_DATA_DIR = GBL_MAP + "data/";
    /*** 基本数据路径地址URL **/
    public static final String MAP_BASE_PATH = "http://mps.amap.com:80/";
    /*** 配置引擎样式文件MapAssert的绝对地址 **/
    public static final String MAP_ASSET_DIR = GBL_MAP + "MapAsset/";

    public static final String TBT_COMMON_CACHE_PATH = GBL_MAP + "tbtCommonComponent/cache";

    public static final String POS_DIR = GBL_MAP + "PosDir" + File.separator;
    // TODO: 2025/2/28 定位测试用
//    public static final String POS_DIR = POS + "PosDir" + File.separator;
}
