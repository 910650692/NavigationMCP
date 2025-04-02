package com.fy.navi.scene.api.route;

public interface ISceneRoutePreference {
    /**
     * 推荐
     * */
    void preferenceRecommend();
    /**
     * 躲避拥堵
     * */
    void preferenceAvoidCongestion();
    /**
     * 少收费
     * */
    void preferenceLessCharge();
    /**
     * 不走高速
     * */
    void preferenceNotHighway();
    /**
     * 高速优先
     * */
    void preferenceFirstHighway();
    /**
     * 大路有限
     * */
    void preferenceFirstMainRoad();
    /**
     * 时间最短
     * */
    void preferenceFastestSpeed();

    /**
     * 关闭场景
     */
    void closeScene();
}
