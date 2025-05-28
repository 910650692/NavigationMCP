package com.fy.navi.burypoint.constant;

public class BuryConstant {

    public static class Model{
        public static final String S_ID = DataTrackerConfig.currentConfig().getsId();
        public static final String S_VID = DataTrackerConfig.currentConfig().getsVid();
        public static final String APP_ID = DataTrackerConfig.currentConfig().getAppId();
    }

    public static class Property {

        public static final String CLASS_NAME = "android.os.SystemProperties";
        public static final String PRODUCTION_SYSTEM_PROPERTY_NAME = "ro.patac.production";

    }

    public static class EventName{
        public static final String AMAP_UNKNOWN = "AMAP_Unknown";
        public static final String AMAP_OPEN = "AMAP_Open";
        public static final String AMAP_HIDE = "AMAP_Hide";
        public static final String AMAP_CLOSE = "AMAP_Close";
        public static final String AMAP_HOME_PREDICTION = "AMAP_Home_Prediction";
        public static final String AMAP_WIDGET_HOME = "AMAP_Widget_Home";
        public static final String AMAP_WIDGET_WORK = "AMAP_Widget_Work";
        public static final String AMAP_WIDGET_SEARCH = "AMAP_Widget_Search";
        public static final String AMAP_WIDGET_NEWROUTE = "AMAP_Widget_NewRoute";
        public static final String AMAP_WIDGET_ENTERAPP = "AMAP_Widget_EnterAPP";
        public static final String AMAP_MAP_SEARCH_LOCATIONTYPE = "AMAP_Map_Search_LocationType";
        public static final String AMAP_MAP_POINT = "AMAP_Map_Point";
        public static final String AMAP_MAP_MYLOCATION = "AMAP_Map_MyLocation";
        public static final String AMAP_DESTINATION_INPUT = "AMAP_Destination_Input";
        public static final String AMAP_DESTINATION_HISTORY_SELECT = "AMAP_Destination_History_Select";
        public static final String AMAP_DESTINATION_LIST_SELECT = "AMAP_Destination_List_Select";
        public static final String AMAP_DESTINATION_OPEN = "AMAP_Destination_Open";
        public static final String AMAP_DESTINATION_GO = "AMAP_Destination_Go";
        public static final String AMAP_DESTINATION_PHONE = "AMAP_Destination_Phone";
        public static final String AMAP_DESTINATION_NEARBY = "AMAP_Destination_Nearby";
        public static final String AMAP_ROUTE_LIST = "AMAP_Route_List";
        public static final String AMAP_ROUTE_PREFERENCE = "AMAP_Route_Preference";
        public static final String AMAP_ROUTE_ADD_MIDDLE = "AMAP_Route_Add_Middle";
        public static final String AMAP_ROUTE_DELETE_MIDDLE = "AMAP_Route_Delete_Middle";
        public static final String AMAP_NAVI_START = "AMAP_Navi_Start";
        public static final String AMAP_NAVI_END_MANUAL = "AMAP_Navi_End_Manual";
        public static final String AMAP_NAVI_END_AUTO = "AMAP_Navi_End_Auto";
        public static final String AMAP_MAP_GASSTATION = "AMAP_Map_GasStation";
        public static final String AMAP_MAP_CHARGINGSTATION = "AMAP_Map_ChargingStation";
        public static final String AMAP_MAP_SETTING = "AMAP_Map_Setting";
        public static final String AMAP_MAP_DESTINATION_CHOOSE = "AMAP_Map_Destination_Choose";
        public static final String AMAP_NAVI_MAP_MANUAL_SLIDE = "AMAP_Navi_Map_Manual_Slide";
        public static final String AMAP_NAVI_MAP_MANUAL_EMPLIFY_SLIDE = "AMAP_Navi_Map_Manual_Emplify_Slide";
        public static final String AMAP_NAVI_MAP_MANUAL_REDUCE_SLIDE = "AMAP_Navi_Map_Manual_Reduce_Slide";
        public static final String AMAP_NAVI_MAP_MANUAL_EMPLIFY_CLICK = "AMAP_Navi_Map_Manual_Emplify_Click";
        public static final String AMAP_NAVI_MAP_MANUAL_REDUCE_CLICK = "AMAP_Navi_Map_Manual_Reduce_Click";
        public static final String AMAP_NAVI_MAP_MANUAL_WAKEUP = "AMAP_Navi_Map_Manual_Wakeup";
        public static final String AMAP_NAVI_MAP_MANUAL_FULLVIEW = "AMAP_Navi_Map_Manual_FullView";
        public static final String AMAP_NAVI_MAP_MANUAL_REFRESHMAP = "AMAP_Navi_Map_Manual_RefreshMap";
        public static final String AMAP_NAVI_MAP_MANUAL_NEWROUTE = "AMAP_Navi_Map_Manual_NewRoute";
        public static final String AMAP_NAVI_MAP_RETURNTOCAR = "AMAP_Navi_Map_ReturnToCar";
        public static final String AMAP_NAVI_POI_ROUTE = "AMAP_Navi_POI_Route";
        public static final String AMAP_NAVI_POI_ROUTE_SELECT = "AMAP_Navi_POI_Route_Select";
        public static final String AMAP_NAVI_VOICE_SELECT = "AMAP_Navi_Voice_Select";
        public static final String AMAP_NAVI_REPORT_SELECT = "AMAP_Navi_Report_Select";
        public static final String AMAP_NAVI_CONTINUE = "AMAP_Navi_Continue";
        public static final String AMAP_HOME_QUICKACCESS = "AMAP_Home_QuickAccess";
        public static final String AMAP_WORK_QUICKACCESS = "AMAP_Work_QuickAccess";
        public static final String AMAP_HOME_SAVE = "AMAP_Home_Save";
        public static final String AMAP_HOME_UNSAVE = "AMAP_Home_Unsave";
        public static final String AMAP_WORK_SAVE = "AMAP_Work_Save";
        public static final String AMAP_WORK_UNSAVE = "AMAP_Work_UnSave";
        public static final String AMAP_TRAFFICRESTRICT_CURRENT = "AMAP_TrafficRestrict_Current";
        public static final String AMAP_TRAFFICRESTRICT_CITY = "AMAP_TrafficRestrict_City";
        public static final String AMAP_FAVORITE_SAVE = "AMAP_Favorite_Save";
        public static final String AMAP_FAVORITE_UNSAVE = "AMAP_Favorite_Unsave";
        public static final String AMAP_FAVORITE_LIST = "AMAP_Favorite_List";
        public static final String AMAP_ACCOUNT_POPUP_BINDINGNOW = "AMAP_Account_Popup_BindingNow";
        public static final String AMAP_ACCOUNT_POPUP_NOREMIND = "AMAP_Account_Popup_NoRemind";
        public static final String AMAP_ACCOUNT_BIND_FINISH = "AMAP_Account_Bind_Finish";
        public static final String AMAP_OPEN_FAIL = "AMAP_Open_Fail";
        public static final String AMAP_CRUISE_ENTER = "AMAP_Cruise_Enter";
        public static final String AMAP_CRUISE_EXIT = "AMAP_Cruise_Exit";
        public static final String AMAP_DESTINATION_ROUTE_SERVICE = "AMAP_Destination_Route_Service";
        public static final String AMAP_DESTINATION_ROUTE_WEATHER = "AMAP_Destination_Route_Weather";
        public static final String AMAP_DESTINATION_ROUTE_WEATHER_AVOID = "AMAP_Destination_Route_Weather_Avoid";
        public static final String AMAP_DESTINATION_ROUTE_GASSTATION = "AMAP_Destination_Route_GasStation";
        public static final String AMAP_DESTINATION_ROUTE_CHARGINGSTATION = "AMAP_Destination_Route_ChargingStation";
        public static final String AMAP_DESTINATION_PARKING_SELECT = "AMAP_Destination_Parking_Select";
        public static final String AMAP_SERVICEAGREEMENT_CHECK = "AMAP_ServiceAgreement_Check";
        public static final String AMAP_PRIVACY_SET = "AMAP_Privacy_Set";
        public static final String AMAP_RETURN_DEFAULT = "AMAP_Return_Default";
        public static final String AMAP_PHONE_DESTINATION_FROM = "AMAP_Phone_Destination_From";
        public static final String AMAP_PHONE_DESTINATION_TO = "AMAP_Phone_Destination_To";
        public static final String AMAP_SETTING_ROUTEPREFERENCE = "AMAP_Setting_RoutePreference";
        public static final String AMAP_SETTING_AVOIDRESTRICT = "AMAP_Setting_AvoidRestrict";
        public static final String AMAP_SETTING_LANELEVELNAVI = "AMAP_Setting_LaneLevelNavi";
        public static final String AMAP_SETTING_MAPCONTENT = "AMAP_Setting_MapContent";
        public static final String AMAP_SETTING_MAPWORDSIZE = "AMAP_Setting_MapWordSize";
        public static final String AMAP_SETTING_CARICON = "AMAP_Setting_CarIcon";
        public static final String AMAP_SETTING_AUTOSCALE = "AMAP_Setting_AutoScale";
        public static final String AMAP_SETTING_CHARGINGPLAN = "AMAP_Setting_ChargingPlan";
        public static final String AMAP_SETTING_VOICEPACKAGE = "AMAP_Setting_VoicePackage";
        public static final String AMAP_SETTING_VOICESTYLE = "AMAP_Setting_VoiceStyle";
        public static final String AMAP_SETTING_CRUISEVOICE = "AMAP_Setting_CruiseVoice";
        public static final String AMAP_SETTING_HOME = "AMAP_Setting_Home";
        public static final String AMAP_SETTING_WORK = "AMAP_Setting_Work";
        public static final String AMAP_SETTING_HOMEWORKSWITCH = "AMAP_Setting_HomeWorkSwitch";
        public static final String AMAP_SETTING_HOT_ADD = "AMAP_Setting_Hot_Add";
        public static final String AMAP_SETTING_FAVORITE_ADD = "AMAP_Setting_Favorite_Add";
        public static final String AMAP_SETTING_CHARGINGSTATION_ADD = "AMAP_Setting_ChargingStation_Add";
        public static final String AMAP_POPUP = "AMAP_Popup";
    }

    public static class ProperType{

        public static final String BURY_KEY_SEARCH_CONTENTS = "G309";
        public static final String BURY_KEY_SETTING_CONTENT = "G524";
        public static final String BURY_KEY_HOME_PREDICTION = "G534";
        public static final String BURY_KEY_TRIP_DISTANCE= "G575";
        public static final String BURY_KEY_ROUTE_PREFERENCE = "G596";
        public static final String BURY_KEY_REMAINING_TIME = "G657";

    }

    public static class Key{
        public static final String ROUTE_POI_TYPE = "RoutePoiType";
        public static final String POI_INFO_ENTRY = "PoiInfoEntity";
    }

    public static class BroadcastMode{
        public static final String CONCISE = "简洁";
        public static final String DETAIL = "详细";
        public static final String MINIMALISM = "极简";
    }

    public static class RoutePreference{
        public static final String RECOMMEND = "高德推荐";
        public static final String AVOID_CONGESTION = "躲避拥堵";
        public static final String LESS_CHARGE = "少收费";
        public static final String NOT_HIGHWAY = "不走高速";
        public static final String FIRST_HIGHWAY = "高速优先";
        public static final String FIRST_MAIN_ROAD = "大路优先";
        public static final String FAST_SPEED = "时间优先";
        public static final String AVOID_CONGESTION_AND_LESS_CHARGE = "躲避拥堵+少收费";
        public static final String AVOID_CONGESTION_AND_NOT_HIGHWAY = "躲避拥堵+不走高速";
        public static final String AVOID_CONGESTION_AND_FIRST_HIGHWAY = "躲避拥堵+高速优先";
        public static final String LESS_CHARGE_AND_NOT_HIGHWAY = "少收费+不走高速";
        public static final String AVOID_CONGESTION_AND_LESS_CHARGE_AND_NOT_HIGHWAY = "躲避拥堵+少收费+不走高速";
        public static final String AVOID_CONGESTION_AND_FIRST_MAIN_ROAD = "躲避拥堵+大路优先";
        public static final String AVOID_CONGESTION_AND_FAST_SPEED = "躲避拥堵+时间优先";
    }

    public static class SearchType{
        public static final String CHARGING = "充电站";
        public static final String GAS = "加油站";
        public static final String PARKING = "停车场";
        public static final String SERVICE = "服务区";
    }

    public static class GuideOption{
        public static final String ROUTE_PREFERENCE = "路线偏好";
        public static final String AVOID_RESTRICT = "避开限行";
        public static final String LEVEL_NAVI = "车道级导航";
        public static final String MAP_CONTENT = "地图显示";
        public static final String MAP_WORD_SIZE = "地图文字大小";
        public static final String CAR_ICON = "个性化车标";
        public static final String AUTO_SCALE = "自动比例尺开关";
        public static final String CHARGING_PLAN = "补能规划开关";
    }

    public static class BroadcastOption{
        public static final String VOICE_PACKAGE = "播报语音";
        public static final String VOICE_STYLE = "导航播报风格";
        public static final String CRUISE_VOICE = "巡航播报设置";
    }

    public static class CarIcon{
        public static final String DEFAULT = "默认";
        public static final String BRAND = "品牌";
        public static final String SPEED = "车速";
    }

    public static class MapFontSize{
        public static final String DEFAULT = "标准字号";
        public static final String BIG = "大字号";
    }

    public static class MapContent{
        public static final String CHARGING_STATION = "充电桩";
        public static final String RTTI = "实时路况";
        public static final String FAVORITE_POINT = "收藏点";
    }

    public static class VoiceStyle{
        public static final String DETAILED = "详细";
        public static final String CONCISE = "简洁";
        public static final String SIMPLE = "极简";
    }

    public static class CruiseVoice{
        public static final String CLOSE = "关闭";
        public static final String ROAD_CONDITION = "前方路况";
        public static final String CAMERA = "电子眼播报";
        public static final String SAFE = "安全提醒";
    }

    public static class CommonText{
        public static final String EXIST = "存在";
        public static final String NOT_EXIST = "不存在";
    }

    public static class Number{
        public static final String ONE = "1";
        public static final String SECOND = "2";
    }

    public enum ZoomAction {
        ZOOM_IN,
        ZOOM_OUT
    }

}
