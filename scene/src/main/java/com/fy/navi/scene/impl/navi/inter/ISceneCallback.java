package com.fy.navi.scene.impl.navi.inter;


import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;

public interface ISceneCallback {

    /**
     * 跳转到沿途搜页面
     */
    default void skipAlongWayFragment() {
    }

    /**
     * 跳转到设置页面
     */
    default void skipSettingFragment() {
    }

    /**
     * 跳转到引导路线偏好scene
     */
    default void skipNaviPreferenceScene() {
    }

    /**
     * 显示/隐藏场景页面
     * @param sceneType 场景类型
     * @param isVisible 是否可见
     */
    default void updateSceneVisible(NaviSceneId sceneType, boolean isVisible) {
    }

    /**
     * @param entity 删除途经点
     */
    default void deleteViaPoint(NaviViaEntity entity) {

    }

    /**
     * 更新导航途经点已到达
     */
    default void onUpdateViaPass() {
    }

    /**
     * 跳转到导航服务区/收费站详情页面
     * @param type service or toll
     * @param sapaInfoEntity 服务区/收费站信息
     */
    default void skipNaviSapaDetailScene(int type, SapaInfoEntity sapaInfoEntity) {

    }

    /***
     * 搜索新的充电站
     */
    default void searchNewChargeStation() {

    }

    /***
     * 打开补能规划
     */
    default void openSupplyPlan() {

    }

    /***
     * 去充电，重新发起新的导航
     */
    default void goCharge() {

    }

    /**
     * 显示控制卡片详情页面
     */
    default void showControlDetails() {

    }

    /**
     * 跳转到搜索页面
     * @param keyWord 搜索关键字
     * @param searchType 搜索类型
     */
    default void goSearchView(final String keyWord, final int searchType) {
    }

    /**
     * 跳转到沿途搜页面
     */
    default void goAlongWayList() {
    }

    /**
     * 关闭搜索页面
     */
    default void closeSearchView() {

    }

    /**
     * @return 因碰撞关闭的消息卡片是否需要再打开 true：需要 false：不需要
     */
    default boolean isNeedCloseNaviChargeTipLater() {
        return false;
    }
}
