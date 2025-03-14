package com.fy.navi.scene.impl.navi.inter;


import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.navi.NaviViaEntity;

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

    default void updateSceneVisible(NaviSceneId sceneType, boolean isVisible) {
    }

    default void deleteViaPoint(NaviViaEntity entity) {

    }

    /**
     * 更新导航途经点已到达
     */
    default void onUpdateViaPass() {
    }
}
