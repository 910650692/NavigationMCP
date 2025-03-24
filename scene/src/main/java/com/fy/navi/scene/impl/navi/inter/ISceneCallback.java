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
}
