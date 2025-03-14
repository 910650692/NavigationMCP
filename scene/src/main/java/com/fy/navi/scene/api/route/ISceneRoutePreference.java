package com.fy.navi.scene.api.route;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public interface ISceneRoutePreference {

    void preferenceRecommend();

    void preferenceAvoidCongestion();

    void preferenceLessCharge();

    void preferenceNotHighway();

    void preferenceFirstHighway();

    void preferenceFirstMainRoad();

    void preferenceFastestSpeed();
}
