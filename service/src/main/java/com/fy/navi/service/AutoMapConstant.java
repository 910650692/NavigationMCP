package com.fy.navi.service;

import androidx.annotation.IntDef;
import androidx.annotation.StringDef;

import com.android.utils.file.FileUtils;

import java.io.File;

public interface AutoMapConstant {
    /*** 沙箱位置 **/
    String SD_PATH = FileUtils.SD_APP_PATH;
    /*** 地图文件缓存位置 **/
    String AUTO_MAP_PATH = SD_PATH + "AutoMap" + File.separator;
    /*** 高德地图文件缓存位置 **/
    String GBL_MAP = AUTO_MAP_PATH + "Gbl" + File.separator;
    // TODO: 2025/2/28 定位测试用
//    String POS = FileUtils.APP_FILE_PATH + "Gbl" + File.separator;

    Float MAP_ZOOM_LEVEL_MAX = 19F;
    Float MAP_ZOOM_LEVEL_MIN = 3F;
    Float MAP_ZOOM_LEVEL_CHANGE_FLAG = 1F;
    Float MAP_ZOOM_LEVEL_DEFAULT = 13F;

    Float MAP_DEFAULT_TEXT_SIZE = 1.3F;

    int PLUG_TYPE_SLOW = 7;
    int PLUG_TYPE_FAST = 9;


    /*** ！！！搜索相关,搜索类型,只能添加不能删除！！！*/
    @IntDef({SearchType.SEARCH_SUGGESTION,
            SearchType.SEARCH_KEYWORD,
            SearchType.POI_SEARCH,
            SearchType.GEO_SEARCH,
            SearchType.ALONG_WAY_SEARCH,
            SearchType.AROUND_SEARCH,
            SearchType.AGGREGATE_SEARCH,
            SearchType.EN_ROUTE_KEYWORD_SEARCH,
            SearchType.COLLECT_SEARCH,
            SearchType.DEEP_INFO_SEARCH,
            SearchType.LINE_DEEP_INFO_SEARCH,
            SearchType.MAP_POINT_SEARCH,
            SearchType.HOME_COMPANY,
            SearchType.COMMON_TO_COLLECT_SEARCH,
            SearchType.GET_POINT_TO_COLLECT_SEARCH,
            SearchType.POI_DETAIL_SEARCH,
    })
    @interface SearchType {
        int SEARCH_SUGGESTION = 0; // 建议搜索
        int SEARCH_KEYWORD = 1; // 关键字搜索
        int POI_SEARCH = 2; // Poi搜索
        int GEO_SEARCH = 3; // 逆地理搜索
        int ALONG_WAY_SEARCH = 4; // 沿途搜索 alongWay
        int AROUND_SEARCH = 5; // 周边搜索
        int AGGREGATE_SEARCH = 6; // 聚合搜索
        int EN_ROUTE_KEYWORD_SEARCH = 7; // 顺路搜索
        int COLLECT_SEARCH = 8; // 收藏
        int DEEP_INFO_SEARCH = 9; // 深度信息搜索
        int LINE_DEEP_INFO_SEARCH = 10; // 沿途批量搜索
        int MAP_POINT_SEARCH = 11; // 地图选点
        int HOME_COMPANY = 12; // 家和公司
        int COMMON_TO_COLLECT_SEARCH = 13; // 常用地址to收藏
        int GET_POINT_TO_COLLECT_SEARCH = 14; // 收到的点to收藏
        int POI_DETAIL_SEARCH = 15; // POI详情搜索，仅支持洗车，美食和景点

    }

    /*** ！！！搜索相关,数据传递key,只能添加不能删除！！！ */
    @StringDef({
            SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE,
            SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD,
            SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST,
            SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE,
            SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE,
            SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL,
            SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_AROUND,
            SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ALONG_WAY,
            SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY,
            SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_COLLECTION,
            SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT,

    })
    @interface SearchBundleKey {
        String BUNDLE_KEY_SEARCH_TYPE = "bundle_key_search_type";
        String BUNDLE_KEY_SEARCH_KEYWORD = "bundle_key_search_keyword";
        String BUNDLE_KEY_SEARCH_POI_LIST = "bundle_key_search_poi_list";
        String BUNDLE_KEY_SEARCH_OPEN_ROUTE = "bundle_key_search_open_route";
        String BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE = "bundle_key_search_open_route_type";
        String BUNDLE_KEY_MSG_PUSH_OPEN_ROUTE_TYPE = "bundle_key_msg_push_open_route_type";
        String BUNDLE_KEY_SEARCH_OPEN_DETAIL = "bundle_key_search_open_detail";
        String BUNDLE_KEY_SEARCH_OPEN_AROUND = "bundle_key_search_open_around";
        String BUNDLE_KEY_SEARCH_OPEN_ALONG_WAY = "bundle_key_search_open_along_way";
        String BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY = "bundle_key_search_open_home_company";
        String BUNDLE_KEY_SEARCH_OPEN_COLLECTION = "bundle_key_search_open_collection";
        String BUNDLE_KEY_SOURCE_FRAGMENT = "bundle_key_source_fragment";
    }

    @interface TrafficEventBundleKey {
        String BUNDLE_KEY_ENTITY = "bundle_key_entity";
    }

    @interface RecordDetailsBundleKey {
        String BUNDLE_RECORD_DERAILS = "bundle_record_details";
    }

    @interface RouteBundleKey {
        String BUNDLE_KEY_START_NAVI_SIM = "bundle_key_route_start_navi_sim";
        String BUNDLE_KEY_ALTER_CHARGE_STATION = "bundle_key_alter_charge_station";
    }

    @interface CommonBundleKey {
        String BUNDLE_KEY_LIMIT_DRIVER = "bundle_key_limit_driver";
        String BUNDLE_KEY_LIMIT_ROUND = "bundle_key_limit_round";
        String BUNDLE_KEY_LIMIT_CITY_SELECTION = "bundle_key_limit_city_selection";
        String BUNDLE_KEY_LIMIT_CITY_TASK_ID = "bundle_key_limit_city_task_id";
    }

    @IntDef({NaviType.NAVI_GPS,
            NaviType.NAVI_SIMULATE})
    @interface NaviType {
        int NAVI_GPS = 0;
        int NAVI_SIMULATE = 1;
    }

    /*** ！！！搜索相关,公共布局 ！！！ */
    @IntDef({SearchLayoutType.SCENE_SUG_SEARCH_FRAGMENT,
            SearchLayoutType.SCENE_SEARCH_RESULT_FRAGMENT,
    })
    @interface SearchLayoutType {
        int SCENE_SUG_SEARCH_FRAGMENT = 0;
        int SCENE_SEARCH_RESULT_FRAGMENT = 1;

    }

    /*** ！！！搜索相关,gbl对应的搜索类型 ！！！ */
    @StringDef({
            SearchQueryType.NORMAL,
            SearchQueryType.AROUND,
            SearchQueryType.ID,
            SearchQueryType.SPQ})
    @interface SearchQueryType {
        String NORMAL = "TQUERY";
        String AROUND = "RQBXY";
        String ID = "IDQ";
        String SPQ = "SPQ";
    }

    /*** ！！！添加关键字历史记录类型！！！ */
    @IntDef({SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY,
            SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY,
    })
    @interface SearchKeywordRecordKey {
        // 关键字记录
        int SEARCH_KEYWORD_RECORD_KEY = 0;
        // 导航记录
        int SEARCH_NAVI_RECORD_KEY = 1;
        // 行程历史记录
        int DRIVING_HISTORY_RECORD_KEY = 2;
    }

    /*** ！！！聚合搜索类型！！！ */
    @IntDef({AggregateKeywordType.UNKNOWN,
            AggregateKeywordType.DINING,
            AggregateKeywordType.SCENIC,
            AggregateKeywordType.MALL,
            AggregateKeywordType.CHARGING,
            AggregateKeywordType.PARKING,
            AggregateKeywordType.BATHROOM,
    })
    @interface AggregateKeywordType {
        int UNKNOWN = 0; // 未知
        int DINING = 1; // 美食
        int SCENIC = 2; // 景点
        int MALL = 4; // 商圈
        int CHARGING = 8; // 充电
        int PARKING = 16; // 停车
        int BATHROOM = 32; // 厕所
    }

    /*** ！！！家和公司类型！！！ */
    @IntDef({HomeCompanyType.COLLECTION,
            HomeCompanyType.HOME,
            HomeCompanyType.COMMON,
            HomeCompanyType.COMPANY,
            HomeCompanyType.ALONG_WAY})
    @interface HomeCompanyType {
        int COLLECTION = 0; // 收藏
        int HOME = 1; // 家
        int COMPANY = 2; // 公司
        int COMMON = 3; //常用地址
        int ALONG_WAY = 4; // 途径点

    }

    /**
     * 收藏夹页面类型数据
     */
    @IntDef({CollectionType.COLLECTION,
            CollectionType.COMMON,
            CollectionType.GET_POINT})
    @interface CollectionType {
        int COLLECTION = 0; // 收藏
        int COMMON = 1; //常用地址
        int GET_POINT = 2; //收到的点

    }

    @interface PoiBundleKey {
        String BUNDLE_KEY_START_POI_TYPE = "bundle_key_start_poi_type";
    }

    /*** ！！！Poi详情页面类型！！！*/
    @IntDef({PoiType.POI_SUGGESTION,
            PoiType.POI_KEYWORD,
            PoiType.POI_HOME,
            PoiType.POI_COMPANY,
            PoiType.POI_COLLECTION,
            PoiType.POI_COMMON,
            PoiType.POI_AROUND,
            PoiType.POI_MAP_CLICK,
            PoiType.POI_DELETE_AROUND
    })
    @interface PoiType {
        int POI_SUGGESTION = 0; // 预搜索
        int POI_KEYWORD = 1; // 关键字搜索
        int POI_HOME = 2; // 添加家
        int POI_COMPANY = 3; // 添加公司
        int POI_COLLECTION = 4; // 添加收藏地址
        int POI_COMMON = 5; // 添加常用地址
        int POI_AROUND = 6; // 添加途径点
        int POI_MAP_CLICK = 7; // 地图点击
        int POI_DELETE_AROUND = 8;//删除途径点
    }

    /*** ！！！沿途批量搜索支持类型！！！ */
    @IntDef({LineDeepQueryType.UNKNOWN,
            LineDeepQueryType.SERVICE_AREA,
            LineDeepQueryType.CHARGING,
            LineDeepQueryType.GAS_STATION,
            LineDeepQueryType.PARK_COMMEND,
    })
    @interface LineDeepQueryType {
        int UNKNOWN = 0;
        int SERVICE_AREA = 1;
        int CHARGING = 2;
        int GAS_STATION = 3;
        int PARK_COMMEND = 4;
    }

    /*** ！！！POI详情样式！！！ */
    @IntDef({
            PointTypeCode.GAS_STATION,
            PointTypeCode.CHARGING_STATION,
            PointTypeCode.CAR_WASH,
            PointTypeCode.CATERING,
            PointTypeCode.PARKING_LOT,
            PointTypeCode.SERVICE_AREA,
            PointTypeCode.SCENIC_SPOT
    })
    @interface PointTypeCode {
        int GAS_STATION = 0;//加油站
        int CHARGING_STATION = 1;//充电站
        int CAR_WASH = 2;//洗车
        int CATERING = 3;//餐饮
        int PARKING_LOT = 4;//停车场
        int SERVICE_AREA = 5;//服务区
        int SCENIC_SPOT = 6;//景点
        int OTHERS = 7;//其他
        int TRANSPORT_HUB = 8;//交通枢纽
    }

    /*** ！！！搜索页面跳转使用 ！！！ */
    @StringDef({
            SourceFragment.MAIN_SEARCH_FRAGMENT,
            SourceFragment.SUG_SEARCH_FRAGMENT,
            SourceFragment.SEARCH_RESULT_FRAGMENT,
            SourceFragment.POI_DETAIL_FRAGMENT,
            SourceFragment.FRAGMENT_COLLECTION,
            SourceFragment.FRAGMENT_HOME_COMPANY,
            SourceFragment.FRAGMENT_AROUND,
            SourceFragment.FRAGMENT_MAIN_ALONG_WAY,
            SourceFragment.FRAGMENT_ALONG_WAY,
    })
    @interface SourceFragment {
        String MAIN_SEARCH_FRAGMENT = "scene_main_search_fragment";
        String SUG_SEARCH_FRAGMENT = "scene_sug_search_fragment";
        String SEARCH_RESULT_FRAGMENT = "scene_search_result_fragment";
        String POI_DETAIL_FRAGMENT = "scene_poi_detail_fragment";
        String FRAGMENT_COLLECTION = "scene_fragment_collection";
        String FRAGMENT_HOME_COMPANY = "scene_fragment_home_company";
        String FRAGMENT_AROUND = "scene_fragment_around";
        String FRAGMENT_MAIN_ALONG_WAY = "scene_main_fragment_along_way";
        String FRAGMENT_ALONG_WAY = "scene_fragment_along_way";
        String FRAGMENT_HOME = "scene_fragment_home";
        String FRAGMENT_COMPANY = "scene_fragment_company";
        String FRAGMENT_COMMON = "scene_fragment_common";
    }

    @interface VoiceKeyWord {
        String BUNDLE_VOICE_KEY_WORD = "bundle_voice_key_word";
    }
}
