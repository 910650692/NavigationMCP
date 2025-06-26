package com.sgm.navi.service;

import com.android.utils.file.FileUtils;

import java.io.File;

public interface GBLCacheFilePath extends AutoMapConstant {
    /*** 外部沙箱位置 **/
    String SD_PATH = FileUtils.getInstance().getEmulatedPhonePath();

    /*** 地图文件缓存位置 sdcard/Android/data/your_package/files/AutoMap/ **/
    String AUTO_MAP_PATH = SD_PATH + "AutoMap" + File.separator;

    /*** 高德地图文件缓存位置 sdcard/Android/data/your_package/files/AutoMap/Gbl/**/
    String GBL_MAP = AUTO_MAP_PATH + "Gbl" + File.separator;

    String GM_LOG_ROOT_PATH = "/data/gmlogger/proclog/";

    /*** sdcard/Android/data/your_package/files/AutoMap/Gbl/libs/ **/
    public static final String DEBUG_LIBS_DIR = GBL_MAP + "libs" + File.separator;

    /*** 车道级回放文件所在目录 sdcard/Android/data/your_package/files/AutoMap/Gbl/loc_replay/ **/
    public static final String GPS_LANELOC_FOLDER = GBL_MAP + "loc_replay/";
    /*** 仿真回放路径 **/
    public static final String MAP_RECORD_PATH = GBL_MAP + "record";
    /*** 车道级导航 自车SR信息测试 **/
    public static final String OFFLINE_LANERESOURCE_DIR = GBL_MAP + "LaneCarSRResource/";

    /*** Auto Map log path **/
    public static final String BLS_LOG = GBL_MAP + "bllog";

    public static final String BLS_COOKIES_PATH = GBL_MAP + "cookie";
    /*** AutoSDK资源文件的原始目录 **/
    public static final String BLS_ASSETS_PATH = "/android_assets/blRes/";
    /*** 配置引擎样式文件MapAssert的绝对地址 **/
    public static final String MAP_ASSET_DIR = BLS_ASSETS_PATH + "MapAsset/";
    /*** 配置引擎样式文件MapAssert的绝对地址 **/
    public static final String MAP_SKY_BOX_ASSET_DIR = "blRes/MapAsset/";
    /*** AutoSDK图层默认资源文件目录 **/
    public static final String BLS_ASSETS_LAYER_PATH = BLS_ASSETS_PATH + "LayerAsset/";

    public static final String BLS_ASSETS_LAYER_CARDS_PATH = "blRes/LayerAsset/DynamicCards/";

    /** 自定义图层卡片锚点资源目录 **/
    public static final String BLS_ASSETS_LAYER_CARD_MARKER_INFO_PATH = "blRes/customRes/style1/cardMarkerInfo/card_marker_info.json";

    /** 自定义图层卡片锚点资源目录 **/
    public static final String BLS_ASSETS_LAYER_CUSTOM_MARKER_INFO_PATH = "blRes/customRes/style1/customMarkerInfo/custom_marker_info.json";

    /*** 自定义图层默认资源文件目录 **/
    public static final String BLS_ASSETS_CUSTOM_STYLE_PATH = "blRes/customRes/style";

    public static final String BLS_ASSETS_CUSTOM_PATH = BLS_ASSETS_PATH + "customRes/style";

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
    public static final String MAP_INDOOR_PATH = "http://m5.amap.com/";


    public static final String TBT_COMMON_CACHE_PATH = GBL_MAP + "tbtCommonComponent/cache";

    public static final String POS_DIR = GBL_MAP + "PosDir" + File.separator;

    /*** 通勤预测数据库缓存目录 sdcard/Android/data/your_package/files/AutoMap/Gbl/wstrDb/ **/
    String BEHAVIOR_WSTR_DB_FILE_DIR = GBL_MAP + "wstrDb" + File.separator;

    /***  暂定 激活文件保存路径  **/
    //sdcard/Android/data/your_package/files/AutoMap/Gbl/activate/
    String ACTIVATE_USER_DATA = GBL_MAP + "activate/";

    String CMB_FILE_NAME = "libcmb_LayerImages.so";
}
