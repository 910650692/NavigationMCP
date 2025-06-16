package com.fy.navi.service;

public interface MapDefaultFinalTag {
    String DEFAULT_TAG = "NaviApp";
    /*** Service Tag **/
     String SERVICE_TAG = "_Service";
    /*** NaviApp_Init_Service **/
     String INIT_SERVICE_TAG = DEFAULT_TAG + "_Init" + SERVICE_TAG;
    /*** NaviApp_Engine_Service **/
     String ENGINE_SERVICE_TAG = DEFAULT_TAG + "_Engine" + SERVICE_TAG;
    /*** NaviApp_Activate_Service **/
    String ACTIVATE_SERVICE_TAG = DEFAULT_TAG + "_Activate" + SERVICE_TAG;
    /*** NaviApp_Map_Service **/
     String MAP_SERVICE_TAG = DEFAULT_TAG + "_Map" + SERVICE_TAG;
    /*** NaviApp_Route_Service **/
     String ROUTE_SERVICE_TAG = DEFAULT_TAG + "_Route" + SERVICE_TAG;
    /*** NaviApp_Navi_Service **/
     String NAVI_SERVICE_TAG = DEFAULT_TAG + "_Navi" + SERVICE_TAG;
     String NAVI_SERVICE_API_IMPL = DEFAULT_TAG + "_Navi" + SERVICE_TAG + "_ApiImpl";
     String NAVI_SERVICE_CALLBACK = DEFAULT_TAG + "_Navi" + SERVICE_TAG + "_Callback";
    /*** NaviApp_Cruise_Service **/
     String CRUISE_SERVICE_TAG = DEFAULT_TAG + "_Cruise" + SERVICE_TAG;
    /*** NaviApp_Position_Service **/
     String POSITION_SERVICE_TAG = DEFAULT_TAG + "_Position" + SERVICE_TAG;
    /*** NaviApp_Speech_Service **/
     String SPEECH_SERVICE_TAG = DEFAULT_TAG + "_Speech" + SERVICE_TAG;
    /*** NaviApp_Layer_Service **/
     String LAYER_SERVICE_TAG = DEFAULT_TAG + "_Layer" + SERVICE_TAG;
    /*** NaviApp_LayerItem_Service **/
     String LAYER_ITEM_SERVICE_TAG = DEFAULT_TAG + "_LayerItem" + SERVICE_TAG;
    /*** NaviApp_MapData_Service **/
     String MAP_DATA_SERVICE_TAG = DEFAULT_TAG + "_MapData" + SERVICE_TAG;
    /*** NaviApp_Search_Service **/
     String SEARCH_SERVICE_TAG = DEFAULT_TAG + "_Search" + SERVICE_TAG;
    /*** NaviApp_Setting_Service **/
     String SETTING_SERVICE_TAG = DEFAULT_TAG + "_Setting" + SERVICE_TAG;
    /*** NaviApp_Favorite_Service **/
     String FAVORITE_SERVICE_TAG = DEFAULT_TAG + "_Favorite" + SERVICE_TAG;
    /*** NaviApp_Account_Service **/
     String ACCOUNT_SERVICE_TAG = DEFAULT_TAG + "_Account" + SERVICE_TAG;
    /*** NaviApp_UserTrack_Service **/
     String USER_TRACK_SERVICE_TAG = DEFAULT_TAG + "_UserTrack" + SERVICE_TAG;
    /*** NaviApp_WeChat_Service **/
     String WE_CHAT_SERVICE_TAG = DEFAULT_TAG + "_WeChat" + SERVICE_TAG;
    /*** NaviApp_Forecast_Service **/
     String FORECAST_SERVICE_TAG = DEFAULT_TAG + "_Forecast" + SERVICE_TAG;
    /*** NaviApp_Group_Service **/
     String GROUP_SERVICE_TAG = DEFAULT_TAG + "_Group" + SERVICE_TAG;
    /*** NaviApp_MsgPush_Service **/
     String MSG_PUSH_SERVICE_TAG = DEFAULT_TAG + "_MsgPush" + SERVICE_TAG;
    /*** NaviApp_Calibration_Service **/
     String CALIBRATION_SERVICE_TAG = DEFAULT_TAG + "_Calibration" + SERVICE_TAG;
    /*** NaviApp_Signal_Service **/
     String SIGNAL_SERVICE_TAG = DEFAULT_TAG + "_Signal" + SERVICE_TAG;
    /*** NaviApp_Signal_ForCast **/
     String FOR_CAST_SERVICE_TAG = DEFAULT_TAG + "ForCast" + SERVICE_TAG;
    /*** NaviApp_Hud_Service **/
    String HUD_SERVICE_TAG = DEFAULT_TAG + "_Hud" + SERVICE_TAG;
    /*** HMI Tag **/
     String HMI_TAG = "_HMI";
    /*** NaviApp_Engine_HMI **/
     String ENGINE_HMI_TAG = DEFAULT_TAG + "_Engine" + HMI_TAG;
    /*** NaviApp_Map_HMI **/
     String MAP_HMI_TAG = DEFAULT_TAG + "_Map" + HMI_TAG;
    /*** NaviApp_Route_HMI **/
     String ROUTE_HMI_TAG = DEFAULT_TAG + "_Route" + HMI_TAG;
    /*** NaviApp_Navi_HMI **/
     String NAVI_HMI_TAG = DEFAULT_TAG + "_Navi" + HMI_TAG;
    /*** NaviApp_Layer_HMI **/
     String LAYER_HMI_TAG = DEFAULT_TAG + "_Layer" + HMI_TAG;
    /*** NaviApp_MapData_HMI **/
     String MAP_DATA_HMI_TAG = DEFAULT_TAG + "_MapData" + HMI_TAG;
    /*** NaviApp_Search_HMI **/
     String SEARCH_HMI_TAG = DEFAULT_TAG + "_Search" + HMI_TAG;
    /*** NaviApp_Setting_HMI **/
     String SETTING_HMI_TAG = DEFAULT_TAG + "_Setting" + HMI_TAG;
    /*** NaviApp_Account_HMI **/
     String ACCOUNT_HMI_TAG = DEFAULT_TAG + "_Account" + HMI_TAG;
    /*** NaviApp_WeChat_HMI **/
     String WECHAT_HMI_TAG = DEFAULT_TAG + "_WeChat" + HMI_TAG;
    /*** NaviApp_Offline_HMI **/
     String OFFLINE_HMI_TAG = DEFAULT_TAG + "_Offline" + HMI_TAG;
    /*** NaviApp_Hud_HMI **/
    String HUD_HMI_TAG = DEFAULT_TAG + "Hud" + HMI_TAG;
     /*** Scene_Tag **/
     String SCENE_TAG = "_Scene";
    /*** NaviApp_Navi_Scene **/
     String NAVI_SCENE_TAG = DEFAULT_TAG + "_Navi" + SCENE_TAG;
     String NAVI_SCENE_PREFERENCE = NAVI_SCENE_TAG + "_Preference";
     String NAVI_SCENE_VIA_LIST = NAVI_SCENE_TAG + "_ViaList";
     String NAVI_SCENE_VIA_LIST_IMPL= NAVI_SCENE_VIA_LIST + "_Impl";
     String NAVI_SCENE_VIA_INFO = NAVI_SCENE_TAG + "_ViaInfo";
     String NAVI_SCENE_VIA_INFO_IMPL = NAVI_SCENE_VIA_INFO + "_Impl";
     String NAVI_SCENE_VIA_DETAIL = NAVI_SCENE_TAG + "_ViaDetail";
     String NAVI_SCENE_VIA_DETAIL_IMPL = NAVI_SCENE_VIA_DETAIL + "_Impl";
     String NAVI_SCENE_VIA_ARRIVE = NAVI_SCENE_TAG + "_ViaArrive";
     String NAVI_SCENE_VIA_ARRIVE_IMPL = NAVI_SCENE_VIA_ARRIVE + "_Impl";
     String NAVI_SCENE_TMC = NAVI_SCENE_TAG + "_Tmc";
     String NAVI_SCENE_TMC_IMPL = NAVI_SCENE_TMC + "_Impl";
     String NAVI_SCENE_TBT = NAVI_SCENE_TAG + "_Tbt";
     String NAVI_SCENE_TBT_IMPL = NAVI_SCENE_TBT + "_Impl";
     String NAVI_SCENE_SPEED = NAVI_SCENE_TAG + "_Speed";
     String NAVI_SCENE_SPEED_IMPL = NAVI_SCENE_SPEED + "_Impl";
     String NAVI_SCENE_SAPA_DETAIL = NAVI_SCENE_TAG + "_SapaDetail";
     String NAVI_SCENE_SAPA_DETAIL_IMPL = NAVI_SCENE_SAPA_DETAIL + "_Impl";
     String NAVI_SCENE_PARALLEL = NAVI_SCENE_TAG + "_Parallel";
     String NAVI_SCENE_PARALLEL_IMPL = NAVI_SCENE_PARALLEL + "_Impl";
     String NAVI_SCENE_LAST_MILE = NAVI_SCENE_TAG + "_LastMile";
     String NAVI_SCENE_LAST_MILE_IMPL = NAVI_SCENE_LAST_MILE + "_Impl";
     String NAVI_SCENE_LANES = NAVI_SCENE_TAG + "_Lanes";
     String NAVI_SCENE_LANES_IMPL = NAVI_SCENE_LANES + "_Impl";
     String NAVI_SCENE_ETA = NAVI_SCENE_TAG +  "_Eta";
     String NAVI_SCENE_ETA_IMPL = NAVI_SCENE_ETA + "_Impl";
     String NAVI_SCENE_CROSS_IMAGE = NAVI_SCENE_TAG + "_CrossImage";
     String NAVI_SCENE_CROSS_IMAGE_IMPL = NAVI_SCENE_CROSS_IMAGE + "_Impl";
     String NAVI_SCENE_CONTROL_MORE = NAVI_SCENE_TAG + "_ControlMore";
     String NAVI_SCENE_CONTROL_MORE_IMPL = NAVI_SCENE_CONTROL_MORE + "_Impl";
     String NAVI_SCENE_CONTROL = NAVI_SCENE_TAG + "_Control";
     String NAVI_SCENE_CONTROL_IMPL = NAVI_SCENE_CONTROL + "_Impl";
     String NAVI_SCENE_CONTINUE = NAVI_SCENE_TAG + "_Continue";
     String NAVI_SCENE_CONTINUE_IMPL = NAVI_SCENE_CONTINUE + "_Impl";
     String NAVI_SCENE_CHARGE_TIP = NAVI_SCENE_TAG + "_ChargeTip";
     String NAVI_SCENE_CHARGE_TIP_IMPL = NAVI_SCENE_CHARGE_TIP + "_Impl";
     String NAVI_SCENE_HANDING_CARD = NAVI_SCENE_TAG + "_HandingCard";
     String NAVI_SCENE_HANDING_CARD_DETAIL = NAVI_SCENE_TAG + "_HandingCardDetail";
     String NAVI_SCENE_HANDING_CARD_IMPL = NAVI_SCENE_HANDING_CARD + "_Impl";
     String NAVI_SCENE_HANDING_CARD_DETAIL_IMPL = NAVI_SCENE_HANDING_CARD_DETAIL + "_Impl";
    /*** NaviApp_Navi_Scene_Sap **/
    String NAVI_SCENE_SAPA = DEFAULT_TAG + "_Navi" + SCENE_TAG + "_Sapa";
    String NAVI_SCENE_SAPA_IMPL = NAVI_SCENE_SAPA + "_Impl";
    /*** NaviApp_Navi_Scene_Cross **/
    String NAVI_SCENE_CROSS = DEFAULT_TAG + "_Navi" + SCENE_TAG + "_Cross";
    String NAVI_HMI_MODEL = NAVI_HMI_TAG + "_Model";
    String NAVI_HMI_VIEW_MODEL = NAVI_HMI_TAG + "_ViewModel";
    String NAVI_HMI_VIEW = NAVI_HMI_TAG + "_Fragment";
    /*** NaviApp_Navi_Bury_Point **/
    String NAVI_BURY_POINT = DEFAULT_TAG + "_Navi" + "Bury_Point";
}
