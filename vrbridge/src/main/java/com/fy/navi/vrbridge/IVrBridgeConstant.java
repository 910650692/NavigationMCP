package com.fy.navi.vrbridge;

public interface IVrBridgeConstant {

    String TAG = "VrBridgeHandle";

    interface NavigationOperateType {
        String START = "START_NAVIGATION";
        String STOP = "END_NAVIGATION";
        String CONTINUE = "CONTINUE_NAVIGATION";
    }

    //搜索目的地类型
    interface DestType {
        String HOME = "HOME";
        String COMPANY = "COMPANY";
        String RESTAURANT = "RESTAURANT";
        String SIGHTS = "SIGHTS";
        String HOTEL = "HOTEL"; //快捷酒店/星级酒店/青年旅社/旅馆/商务酒店/招待所/酒店/宾馆
        String GAS_STATION = "GAS_STATION"; //加油站
        String CHARGING_STATION = "CHARGING_STATION";//充电站/充电桩
        String PARKING = "PARKING";//停车场/停车位
        String ATM = "ATM";//ATM/取款机/自动取款机
        String BANK = "BANK";//银行/银行营业厅
        String SHOPPING = "SHOPPING";//购物中心/书店/数码家电/眼镜店/商场
        String SUPERMARKET = "SUPERMARKET"; //超市/大型超市/便利店
        String CAR_WASH = "CAR_WASH"; //洗车店/洗车房
        String AIRPORT = "AIRPORT"; //机场
        String TRAIN_STATION = "TRAIN_STATION"; //火车站
        String SERVICE_STATION = "SERVICE_STATION"; //服务区/服务站
        String HOSPITAL = "HOSPITAL"; //医院/诊所/门诊/急诊
        String ENTERTAINMENT = "ENTERTAINMENT"; //休闲娱乐/网吧/酒吧/洗浴/足疗/按摩/澡堂/棋牌室/茶馆/体育场馆/游乐场
        String KTV = "KTV"; //KTV/唱吧
        String CINEMA = "CINEMA"; //电影院
        String TOILET = "TOILET"; //卫生间/厕所/洗手间/WC
        String BEAUTY = "BEAUTY"; //丽人/美发/美容/美甲/美体
    }

    //导航去目的地的路线偏好
    interface RouteType {
        String NOT_HIGHWAY = "CHOOSE_NOHIGHWAY_ROUTE"; //不走高速
        String AVOID_JAM = "CHOOSE_AVOID_JAM"; //躲避拥堵
        String FIRST_HIGHWAY = "CHOOSE_HIGHWAY_ROUTE"; //高速优先
        String LESS_CHARGE = "CHOOSE_CHEAPER_ROUTE"; //少收费
        String FAST_SPEED = "CHOOSE_FASTER_ROUTE"; //耗时最少
        String PREFER_RECOMMEND = "CHOOSE_RECOMMEND_ROUTE"; //智能推荐
        String AUTO_ROUTE = "CHOOSE_AUTOP_ROUTE"; //智驾优先（当前SD不支持此偏好）
    }

    //检索条件的key值
    interface ConditionKey {
        String DISTANCE = "distance"; //距离
        String PRICE = "price"; //价格
        String RATE = "rate"; //评价
        String SERVICES = "service"; //服务
        String LEVEL = "level"; //水平
        String CENTER = "center"; //中心
    }

    interface DistanceValue {
        String NEAREST  = "NEAREST"; //最近的
        String FURTHEST = "FURTHEST"; //最远的
        String NEARBY = "NEARBY"; //附近的
    }

    String RATE_HIGH = "HIGH";
    String PRICE_CHEAP = "CHEAP";

    //语音内部使用，判断搜索类型，方便收到第一次搜索结果后续操作
    interface VoiceSearchType {
        int DEFAULT = -1;
        int ONLY_KEYWORD = 0; //仅只有关键字搜索
        int WITH_PREFER = 1; //带算路偏好
        int WITH_CONDITION = 2; //多条件深度搜索
        int WITH_PASS_BY = 3; //带途径点
        int ALONG_WAY = 4; //沿途搜
        int SET_HOME_COMPANY = 5; //设置家/公司
        int SHOW_POI_DETAIL = 6; //查询当前位置，展示POI详情
        int ADD_FAVORITE = 7; //收藏搜到的结果
        int TIME_AND_DIST = 8; //根据搜索结果获取到目的地的距离与时间
        int CONDITION_IN_PAGE = 9; //多条件搜索跳转到搜索页面
        int POI_SORT = 10; //POI排序
    }

    String CURRENT_LOCATION = "CURRENT_LOCATION";

    //语音指令需要打开的界面
    interface VoiceIntentPage {
        int KEYWORD_SEARCH = 1; //关键字搜
        int AROUND_SEARCH = 2; //周边搜
        int ROUTING = 3; //路线规划
        int ALONG_SEARCH = 4; //导航沿途搜
        int SEARCH_HISTORY = 5; //历史记录
        int FAVORITE_PAGE = 6; //收藏夹
        int START_NAVIGATION = 7; //发起引导
        int POI_DETAIL = 8; //展示Poi详情
        int HOME_COMPANY_SET = 9; //设置家-公司地址
        int SELECT_ROUTE = 10; //切换选中的路线
        int CLOSE_CURRENT_PAGE = 11; //关闭当前界面
    }

    interface VoiceIntentParams {
        String INTENT_PAGE = "voice_page"; //需要打开的界面
        String KEYWORD = "keyword"; //搜索关键字
        String AROUND_POINT = "center_point"; //周边搜中心点
        String ROUTE_REQUEST = "speech_open_route"; //打开路线规划
        String POI_DETAIL_INFO = "poi_info"; //打开POI详情需要展示的poi信息
        String HOME_COMPANY_TYPE = "home_company"; //家或公司
        String ROUTE_INDEX = "route_index"; //选择路线时的index，从0开始
        String AROUND_RADIUS = "radius"; //周边搜索半径，单位米
    }


    interface PoiSelectType {
        String POSITION = "position";
        String DISTANCE = "distance";
        String PRICE = "price";
        String RATE = "rate";
    }

    interface PoiSortType {
        String DISTANCE = "DISTANCE";
        String PRIZE = "PRIZE";
        String RATE = "RATE";
    }

    interface PoiSortValue {
        String PRIORITY_RECOMMEND = "推荐排序";
        String PRIORITY_DISTANCE = "距离优先";
        String PRIORITY_RATE = "好评优先";
        String PRIORITY_LOW_PRICE = "低价优先";
        String PRIORITY_HIGH_PRICE = "高价优先";
    }

    interface PoiSortRule {
        String ASCENDING = "ASCENDING"; //升序
        String DESCENDING = "DESCENDING"; //降序
    }

    interface PoiType {
        String CURRENT_LOCATION = "LOCATION"; //当前位置
        String DESTINATION = "DESTINATION"; //目的地
        String COMMON = "common_poi"; //普通Poi
        String PASS_BY = "PASSBY"; //途径点
    }

    //平行路切换
    interface ParallelOption {
        String MAIN = "MAIN";
        String SIDE = "SIDE";
        String ON = "ON";
        String UNDER = "UNDER";
    }

    interface ThemeMode {
        int AUTO = 16;
        int DAY = 17;
        int NIGHT = 18;
    }

    interface MapToggleAction {
        String OPEN = "OPEN";
        String CLOSE = "CLOSE";
    }

    String ROAD_CONDITION = "路况";

    String ROUTE_HINT = "的路线";

    interface MapMode {
        String CAR_2D = "2D_FOLLOW_LOGO"; //2D车头朝上
        String NORTH_2D = "2D"; //2D正北朝上
        String CAR_3D = "3D"; //3D车头朝上
        String DEFAULT = "DEFAULT"; //收到此参数默认转为下一个模式
    }

}
