package com.sgm.navi.vrbridge;

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
        String POI_COLLECT = "poiCollect";
        String NAVI_TO_HOME = "NAVI_TO_HOME"; //导航回家地址未设置
        String NAVI_TO_COMPANY = "NAVI_TO_COMPANY";//导航去公司地址未设置
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
        String MAIN_ROUTE = "CHOOSE_MAINROAD_ROUTE";
        String NOT_SUPPORT_TYPE = "不支持的偏好类型";
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
        String NEAREST = "NEAREST"; //最近的
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
        int START_ARRIVAL_INFO = 11; //两点之前的时长和距离，与到家/公司分开处理
        int NAVI_TO_HOME_COMPANY = 12; //导航回家/去公司，先设置地址
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
        int MOVE_TO_BACK = 12; //应用退到后台
        int COLLECT_COMMON = 13; //收藏普通点
        int TURN_TARGET = 14; //搜索结果翻页目标页面
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
        String IS_END = "IS_END"; //关键字搜是否作为途径点
        String TARGET_PAGE = "target_page"; //翻页目标页面
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
        String RECOMMEND = "RECOMMEND";
    }

    interface PoiSortValue {
        String PRIORITY_RECOMMEND = "推荐排序";
        String PRIORITY_DISTANCE = "距离优先";
        String PRIORITY_RATE = "好评优先";
        String PRIORITY_LOW_PRICE = "低价优先";
        String PRIORITY_HIGH_PRICE = "高价优先";
        String PRIORITY_DEFAULT = "综合排序";
    }

    interface PoiSortRule {
        String ASCENDING = "ASCENDING"; //升序
        String DESCENDING = "DESCENDING"; //降序
    }

    interface PoiType {
        String CURRENT_LOCATION = "CURRENT_LOCATION"; //当前位置
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

    interface VoiceMapMode {
        String CAR_2D = "2D_FOLLOW_LOGO"; //2D车头朝上
        String NORTH_2D = "2D"; //2D正北朝上
        String CAR_3D = "3D"; //3D车头朝上
        String DEFAULT = "DEFAULT"; //收到此参数默认转为下一个模式
    }

    //语音传递的引导播报模式
    interface VoiceBroadcastMode {
        String DETAILED = "DETAILED"; //详细
        String BRIEF = "BRIEF"; //简洁
        String MINIMALIST = "MINIMALIST"; //极简
    }

    //地图未打开，语音输入指令，等打开加载完成后需要继续执行的动作
    interface VoiceCommandAction {
        String OPEN_FAVORITE = "open_favorite"; //打开收藏界面
        String OPEN_HISTORY = "open_history"; //打开历史记录
        String COLLECT_COMMON = "collect_poi"; //收藏指定poi
        String CHANGE_VIEW = "map_view"; //切换地图视角
        String ZOOM_LEVEL = "zoom_level"; //缩放底图
        String ROUTE_NAVIGATION = "route_navi"; //搜索导航意图
    }

    interface ResponseString {
        String CANCLE_SUCCESS = "取消成功";

        String ALREADY_IN = "当前已为";
        String IN_ALREADY = "已切换为";

        String NOT_SUPPORT_ZOOM_COMMAND = "不支持的的缩放指令";
        String ALREADY_MAX_SIZE = "当前已为最大地图";
        String ALREADY_MIN_SIZE = "当前已为最小地图";
        String ZOOM_TO_MAX = "已放大地图到最大";
        String ZOOM_TO_MIN = "已缩小地图到最小";
        String ZOOM_IN = "地图已放大";
        String ZOOM_OUT = "地图已缩小";

        String ALREADY_SHOW_PREVIEW = "当前已展示路线全览";
        String ALREADY_EXIT_PREVIEW = "当前未展示路线全览";
        String SHOW_PREVIEW = "已展示路线全览";
        String EXIT_PREVIEW = "已退出路线全览";
        String NAVI_CANT_SHOW_PREVIEW = "当前不在导航状态，无法查看路线全览哦";

        String NOT_SUPPORT_LIGHT_NAVI = "暂不支持切换经典导航";
        String NOT_SUPPORT_THIS_FUNCTION = "不支持此功能";
        String NOT_SUPPORT_OPERATE = "不支持的指令类型";

        String ALREADY_IN_NAVI = "当前已在导航中";
        String LETS_GO = "开始出发";
        String WHERE_TO_GO = "请问你要去哪里呢";
        String NAVI_END = "导航结束";
        String NOT_IN_NAVI = "当前不在导航状态";
        String RESUME_NAVI = "已恢复导航";
        String OK = "好的";
        String NOT_SUPPORT_NAVI_COMMAND = "不支持的的导航指令";

        String NOT_SUPPORT_VR_CTRL = "该功能暂不支持语音控制";

        String EMPTY_ROAD_NAME = "指定道路名称为空";

        String NOT_SUPPORT_AFTER_ADD_VIA = "添加途经点后不支持该功能";

        String PLEASE_NAVI_FIRST = "请先发起导航";

        String CANT_FIND_WAY_TO_GO = "没有找到走";
        String CANT_FIND_WAY_NOT_GO = "没有找到不走";
        String RATIONAL_ROUTE = "的合理路线";
        String INFORMATION = "的信息";

        String CURRENT_ROUTE_INCLUDE_ROAD = "当前线路已通过此道路";

        String ALREADY_REFRESH_ROUTE = "已为你更新路线";

        String ALREADY_SWITCH_PREFERENCE = "已切换路线偏好";
        String NOT_HIGHWAY = "不走高速";
        String AVOID_JAM = "躲避拥堵";
        String FIRST_HIGHWAY = "高速优先";
        String LESS_CHARGE = "少收费";
        String FAST_SPEED = "时间优先";
        String PREFER_RECOMMEND = "智能推荐";
        String MAIN_ROUTE = "大路优先";

        String ALREADY_IS = "当前已是";
        String STH_ROUTE = "的路线";
        String ALREADY_USE = "已使用";
        String PLAN = "方案";

        String ROUTE_RESULT_VOID = "当前路线规划结果为空";
        String NOT_IN_RANGE = "不在选择范围内";

        String ALREADY_CHOOSE = "已选择第";
        String ROUTE_NUM = "条路线";
        String NOT_SUPPORT_CHOOSE_ROUTE = "不支持选则路线";

        String NOT_SUPPORT_SETTING_TYPE = "不支持的设置类型";

        String CANT_LOCATE_POS = "不好意思，我定位不到你在哪里";

        String SWITCH_MAIN_ROAD = "已切换到主路";
        String ALREADY_MAIN_ROAD = "当前已在主路";
        String SWITCH_SUB_ROAD = "已切换到辅路";
        String ALREADY_SUB_ROAD = "当前已在辅路";
        String NO_SUPPORT_OTHER_ROAD_OPTION = "不支持主路和辅路之外的选项";
        String NO_ROAD_TO_SWITCH = "当前没有主辅路可切换";
        String IN_PARALLEL_ROAD = "正在切换主辅路，请稍后再试";
        String IN_PARALLEL_BRIDGE = "正在切换上下桥，请稍后再试";
        String FIRST_NAVI_PLEASE = "先发起导航才能切换主辅路";

        String SWITCH_MAIN_BRIDGE = "已切换到桥上";
        String ALREADY_MAIN_BRIDGE = "当前已在桥上";
        String SWITCH_SUB_BRIDGE = "已切换到桥下";
        String ALREADY_SUB_BRIDGE = "当前已在桥下";
        String NO_SUPPORT_OTHER_BRIDGE_OPTION = "不支持桥上和桥下以外的选项";
        String NO_BRIDGE_TO_SWITCH = "当前没有桥上下可切换";
        String FIRST_NAVI_PLEAS = "先发起导航才能切换桥上下";

        String CURRENT_SPEED_LIMIT = "当前道路限速";
        String CAUTION_DRIVE = "km/h，请小心驾驶";
        String NO_LIMIT_INFO = "前方暂无限速信息，不过也不要开的太快哦";
        String NO_NAVI_NO_INFO = "当前不在导航状态，没有行程相关信息哦";

        String DEST_REMAIN_LIGHT = "离目的地剩余";
        String TRAFFIC_LIGHT = "个红绿灯";
        String NO_LIGHT_INFO = "暂无红绿灯信息，请稍后再试";
        String NO_EMPTY_DEST = "目的地不可为空";
        String DEST_REMAIN_DIST = "距目的地还有";
        String APART_FIRST_VIA = "距第一个途径点还有";
        String DEST_REMAIN_TIME = "到目的地大约需要";
        String APART_FIRST_POI = "距第一个途径点大约需要";
        String NO_INFO = "没有查询到相关信息，请稍后重试";

        String NET_TRASH = "当前网路状态不佳，请稍后再试";

        String ADD_HOME_BEFORE_GET = "未找到家的地址，先去添加吧";
        String ADD_COMPANY_BEFORE_GET = "未找到公司的地址，先去添加吧";
        String ADD_HOME_WITH_SAY = "未找到家的地址，先去添加吧，试试说：设置家的地址";
        String ADD_COMPANY_WITH_SAY = "未找到公司的地址，先去添加吧，试试说：设置公司的地址";
        String CANT_FIND = "未找到";
        String TRY_OTHER = ",试试别的吧";

        String NO_INFO_TRY_OTHER = "没有查询到相关信息，试试别的吧";

        String PASS_BY_PARAM_EMPTY = "沿途搜参数为空";
        String NAVI_BEFORE_PASSBY_ROUTE = "需要发起导航，才能帮你规划沿途的路线，试试说：导航回家";
        String OFFLINE_NOT_SUPPORT = "当前使用离线算路，不支持该功能";

        String BRIEF = "简洁播报模式";
        String DETAILED = "详细播报模式";
        String MINIMALIST = "极简模式";
        String NOT_SUPPORT_BROADCAST = "不支持的播报模式";

        String NORTH_2D = "正北模式";
        String CAR_2D = "2D模式";
        String NOT_SUPPORT_MODE = "不支持的地图模式";

        String EMPTY_FAVORITE = "收藏目的地为空";
        String GET_DEST_INFO_FAIL = "获取目的地信息失败";
        String CANT_GET_LOCAL_INFO = "不好意思，无法获取当前定位信息";
        String NOT_SUPPORT_FAVORITE_TYPE = "不支持的收藏类型";

        String OPEN_FAVORITE = "已打开收藏地址";
        String CANT_OPEN_FAVORITE_NAVI = "导航中无法打开收藏地址";
        String OPEN_HISTORY = "已打开收藏地址";
        String CANT_OPEN_HISTORY_NAVI = "导航中无法打开收藏地址";

        String NOT_SUPPORT_FILTER_TYPE = "不支持的筛选类型";

        String ALREADY_OPEN_MAP = "地图已打开";
        String ALREADY_CLOSE_MAP = "地图已关闭";

        String NOT_SUPPORT_TEAM = "不支持组队相关功能";

        String NAVI_VOICE_ALREADY_CLOSED = "当前导航声音已关闭";
        String NAVI_VOICE_CLOSED = "已关闭导航声音";
        String NAVI_VOICE_ALREADY_OPENED = "当前导航声音已关闭";
        String NAVI_VOICE_OPENED = "已关闭导航声音";

        String MANEUVER_ICON_TURN_LEFT = "下一个路口需要左转";
        String MANEUVER_ICON_TURN_RIGHT = "下一个路口需要右转";
        String MANEUVER_ICON_SLIGHT_LEFT = "下一个路口向左前方行驶";
        String MANEUVER_ICON_SLIGHT_RIGHT = "下一个路口向右前方行驶";
        String MANEUVER_ICON_TURN_HARD_LEFT = "下一个路口向左后方行驶";
        String MANEUVER_ICON_TURN_HARD_RIGHT = "下一个路口向右后方行驶";
        String MANEUVER_ICON_U_TURN = "下一个路口左转掉头";
        String MANEUVER_ICON_CONTINUE = "下一个路口直行";
        String MANEUVER_ICON_WAY = "前方到达途径点";
        String MANEUVER_ICON_ENTRY_RING = "下一个路口右拐驶入环岛";
        String MANEUVER_ICON_LEAVE_RING = "下一个路口右拐驶出环岛";
        String MANEUVER_ICON_SAPA = "前方到达服务区";
        String MANEUVER_ICON_TOLLGATE = "前方到达收费站";
        String MANEUVER_ICON_DESTINATION = "前方到达目的地";
        String MANEUVER_ICON_TUNNEL = "前方进入隧道";
        String MANEUVER_ICON_ENTRY_LEFT_RING = "下一个路口左拐驶入环岛";
        String MANEUVER_ICON_LEAVE_LEFT_RING = "下一个路口左拐驶出环岛";
        String MANEUVER_ICON_U_TURN_RIGHT = "下一个路口右转掉头";
        String MANEUVER_ICON_SPECIAL_CONTINUE = "前方顺行路段，无需转向";
        String MANEUVER_ICON_ENTRY_RING_LEFT = "前方右转驶入环岛后，需左转驶出环岛";
        String MANEUVER_ICON_ENTRY_RING_RIGHT = "前方右转驶入环岛后，需右转驶出环岛";
        String MANEUVER_ICON_ENTRY_RING_CONTINUE = "前方右转驶入环岛后，需直行驶出环岛";
        String MANEUVER_ICON_ENTRY_RING_U_TURN = "前方右转驶入环岛后，需掉头驶出环岛";
        String MANEUVER_ICON_ENTRY_LEFT_RING_LEFT = "前方左转驶入环岛后，需左转驶出环岛";
        String MANEUVER_ICON_ENTRY_LEFT_RING_RIGHT = "前方左转驶入环岛后，需右转驶出环岛";
        String MANEUVER_ICON_ENTRY_LEFT_RING_CONTINUE = "前方左转驶入环岛后，需直行驶出环岛";
        String MANEUVER_ICON_ENTRY_LEFT_RING_U_TURN = "前方左转驶入环岛后，需掉头驶出环岛";
        String MANEUVER_ICON_ENTRY_RING1 = "驶入环岛入口1";
        String MANEUVER_ICON_ENTRY_RING2 = "驶入环岛入口2";
        String MANEUVER_ICON_ENTRY_RING3 = "驶入环岛入口3";
        String MANEUVER_ICON_ENTRY_RING4 = "驶入环岛入口4";
        String MANEUVER_ICON_ENTRY_RING5 = "驶入环岛入口5";
        String MANEUVER_ICON_ENTRY_RING6 = "驶入环岛入口6";
        String MANEUVER_ICON_ENTRY_RING7 = "驶入环岛入口7";
        String MANEUVER_ICON_ENTRY_RING8 = "驶入环岛入口8";
        String MANEUVER_ICON_ENTRY_RING9 = "驶入环岛入口9";
        String MANEUVER_ICON_ENTRY_RING10 = "驶入环岛入口10";
        String MANEUVER_ICON_LEAVE_RING1 = "驶出环岛出口1";
        String MANEUVER_ICON_LEAVE_RING2 = "驶出环岛出口2";
        String MANEUVER_ICON_LEAVE_RING3 = "驶出环岛出口3";
        String MANEUVER_ICON_LEAVE_RING4 = "驶出环岛出口4";
        String MANEUVER_ICON_LEAVE_RING5 = "驶出环岛出口5";
        String MANEUVER_ICON_LEAVE_RING6 = "驶出环岛出口6";
        String MANEUVER_ICON_LEAVE_RING7 = "驶出环岛出口7";
        String MANEUVER_ICON_LEAVE_RING8 = "驶出环岛出口8";
        String MANEUVER_ICON_LEAVE_RING9 = "驶出环岛出口9";
        String MANEUVER_ICON_LEAVE_RING10 = "驶出环岛出口10";
        String MANEUVER_ICON_MERGE_LEFT = "前方需靠左行驶";
        String MANEUVER_ICON_MERGE_RIGHT = "前方需靠右行驶";
        String UNKNOWN_ROAD_INFO = "未知道路信息，请稍后再试";

        String OFFLINE_CANT_SEARCH = "离线导航，路况查询不可用";
        String NO_ROAD_INFO = "暂无路况信息，请稍后再试";

        String ABNORMAL_POI = "地点信息异常，请重试";
        String ONLY_SUPPORT_POI_ON_ROUTE = "仅支持导航路线上的路况信息查询哦";
        String NO_ROUTE_INFO = "缺少线路信息，请重试";
        String ROAD_CONDITION = "路况";
        String CURRENT_POS = "当前位置到";

        String SMOOTH = "畅通";
        String LITTLE_SMOOTH = "部分缓行";
        String BLOCK = "部分拥堵";
        String SEVER_BLOCK = "部分拥堵";
        String SEVER_SMOOTH = "极度通畅";
        String NO_ROAD_CONDITION = "暂未查询到相应路况信息，请稍后再试";
        String UNKNOWN_ROAD_CONDITION = "未知状态，请稍后再试";
        String UNKNOWN_SEARCH_PARAM = "未知查询条件，请稍后再试";

        String EMPTY_SEARCH_CALLBACK = "空的搜索结果回调";
        String EMPTY_DEST = "目的地为空";
        String NOT_SUPPORT_INTENTION = "不支持的搜索/导航意图";
        String DETAIL_SEARCH_NO_RESULT = "详情搜无返回结果";
        String NO_RESULT_TRY_OTHER = "未找到相关结果，试试别的吧";
        String EMPTY_RESULT_TRY_OTHER= "结果为空，试试别的吧";
        String SEARCH = "搜索";
        String MULTI_DEST_EMPTY = "多目的地导航途径点信息为空";
        String PROCESSING_MULTI_DEST = "正在顺序执行多目的处理";
        String OUT_OF_CHOICE_RANGE = "超出选择范围";
        String LAST_SEARCH_RESULT_EMPTY = "上一轮搜索结果为空";
        String SESSION_ID_NOT_MATCH = "sessionId不匹配";
        String NOT_SUPPORT_COMMENT = "不支持的指令";
        String NO_PROPER_DEST = "没找到符合条件的地点";
        String SEARCH_CONDITION_EMPTY = "检索条件为空";
        String CANT_GET_POS_INFO = "无法获取当前定位信息，请稍后重试";
        String CANT_GET_LOCATION = "无法获取当前位置，请稍后重试";
        String CURRENT_LOCATE = "当前定位";
        String AROUND = "附近";
        String ADD_FAVORITE = "已为你收藏";
        String CANT_ADD_EMPTY_POI = "空的poi名称，无法收藏";
        String ALREADY_SAVED = "此点已经被收藏";
        String NO_SUCH_INFO = "暂未查询到相应信息，请稍后再试";
        String NOT_SUPPORT_CURRENT_SORT = "不支持该排序，请继续选择";
        String NOT_SUPPORT_SORT_TYPE = "不支持的排序类型";
        String SORT_RESULT_EMPTY = "排序搜索结果为空";

        String NO_SUITABLE_VIA = "没有找到符合的途径点";
        String CAN_NOT_DELETE_VIA = "不处于选路态或导航态，无法删除途径点";
        String VIA_DELETE_HINT = "已删除途径点，正在重新规划路线";

        String HAVE_NO_POI_PAGE = "当前没有展示搜索结果页面";
        String ALREADY_FIRST_PAGE = "当前已是第一页";
        String ALREADY_LAST_PAGE = "当前已是最后一页";
        String ALREADY_CURRENT_PAGE = "已显示此页面";
        String PAGE_ERROR_PARAM = "错误的参数";

        String ROUTE_CHARGE_NO_SUPPORT = "当前无补能规划相关信息";
        String ROUTE_CHARGE_SEARCH = "已搜索补能点";
        String ROUTE_CHARGE_REFRESH = "已刷新路线";

        String UN_SUPPORT_SORT = "当前搜索结果不支持排序";

        String UN_SUPPORT_IN_NAVI = "导航态不支持此指令";

        String ROAD_ASSIGN_DONE = "搞定";
        String ROAD_ASSIGN_NO = "收到";
    }

    interface PoiPageType {
        String DIRECTION = "direction";
        String INDEX = "index";
        String REVERSE_INDEX = "index_reverse";
    }

    interface PageTypeValue {
        String DOWN = "DOWN";
        String UP = "UP";
    }

    interface VrExportPage {
        int ROAD_CONDITION = 8; //打开或关闭路况
        int ZOOM_LEVEL = 9; //地图缩放
    }

    String CHANGING_ROUTE_CONFIRM = "UPDATE_CHANGING_ROUTE";

    interface RouteChargeType {
        int SEARCH = 3;
        int REFRESH = 5;
    }

    String HIDE_OVERLAY_ACTION = "patac.intent.ACTION_BROADCAST_HIDE_ALLAPPS";
    String PKG_SUSTEMUI = "com.android.systemui";

    interface MultipleRoundAction {
        String CONFIRM = "CONFIRM";
        String NO = "CONFIRM_NO";
    }
    interface RoadAssignAction {
        String ASSIGN = "ASSIGNED_ROAD";
        String NON_ASSIGN = "NON_ASSIGNED_ROAD";
    }

}
