package com.fy.navi.scene.ui.navi.manager;

/**
 * 导航Scene类型定义
 *
 * @author fy
 * @version $Revision.*$
 */
public enum NaviSceneId {
    NAVI_SCENE_2D_CROSS(0),// 2D路口大图
    NAVI_SCENE_3D_CROSS(1),// 3D路口大图
    NAVI_SCENE_ETA(2),//eta信息
    NAVI_SCENE_LANES(3),//车道线信息
    NAVI_SCENE_TBT(4),//tbt信息
    NAVI_SCENE_VIA_POINT_LIST(5),//途经点列表
    NAVI_SCENE_VIA_POINT_ARRIVE(6),//(暂时闲置)
    NAVI_SCENE_VIA_DETAIL_INFO(7),//途经点tab显示
    NAVI_SCENE_SERVICE_AREA(8),//服务区/收费站
    NAVI_SCENE_CONTROL(9),//控制tools
    NAVI_SCENE_PARALLEL(10),//平行路切换
    NAVI_SCENE_CONTROL_MORE(11),//控制tools更多
    NAVI_SCENE_PREFERENCE(12),//路线偏好
    NAVI_SCENE_SPEED(13),//区间测速
    NAVI_SCENE_TMC(14),//路况条信息
    NAVI_SCENE_CUR_ROAD(15),//当前道路名
    NAVI_VIA_ARRIVED_POP(16),//途经点到达弹窗
    NAVI_SAPA_DETAIL_INFO(17),//服务区/收费站详情页面
    NAVI_DRIVE_REPORT(18),//行车报告
    NAVI_SCENE_LAST_MILE(19),//最后一公里
    NAVI_CHARGE_TIP(20),// EvCar消息
    NAVI_SUSPEND_CARD(21),// 悬挂卡
    NAVI_CONTINUE(22),// 继续导航
    NAVI_SUSPEND_CARD_DETAIL(23); // 悬挂卡列表详情
    NaviSceneId(final int i) {

    }
}
