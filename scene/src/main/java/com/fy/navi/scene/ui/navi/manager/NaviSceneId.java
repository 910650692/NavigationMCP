package com.fy.navi.scene.ui.navi.manager;

/**
 * 导航Scene类型定义
 */
public enum NaviSceneId {
    NAVI_SCENE_2D_CROSS(0),// 2D路口大图
    NAVI_SCENE_3D_CROSS(1),// 3D路口大图
    NAVI_SCENE_ETA(2),//eta信息
    NAVI_SCENE_LANES(3),//车道线信息
    NAVI_SCENE_TBT(4),//tbt信息
    NAVI_SCENE_VIA_POINT_FOLD(5),//途经点收起
    NAVI_SCENE_VIA_POINT_UNFOLD(6),//途经点展开
    NAVI_SCENE_VIA_POINT_ARRIVE(7),//途经点到达
    NAVI_SCENE_VIA_DETAIL_INFO(8),//详情卡片/交通事件
    NAVI_SCENE_SERVICE_AREA(9),//服务区/收费站
    CARD_CONTINUE_NAVI_BTN(10),//继续导航按钮
    NAVI_SCENE_PARALLEL(11),//平行路切换
    NAVI_SCENE_PARK_LIST(12),//停车场列表
    NAVI_SCENE_CONTROL(13),//控制tools
    NAVI_SCENE_LAST_MILE(14),//最后一公里
    NAVI_SCENE_PREFERENCE(15),//路线偏好
    NAVI_SCENE_SPEED(16),//区间测速
    NAVI_SCENE_TMC(17),//路况条信息
    NAVI_SCENE_CUR_ROAD(18),//当前道路名
    NAVI_VIA_ARRIVED_POP(19);//途经点到达弹窗

    NaviSceneId(int i) {

    }
}
