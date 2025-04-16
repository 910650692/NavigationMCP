package com.fy.navi.scene.api.route;

public interface ISceneRoutePreference {
    /**
     * 推荐
     * @param source 0:从设置页进入，1：其他
     * */
    void preferenceRecommend(int source);
    /**
     * 躲避拥堵
     * @param source 0:从设置页进入，1：其他
     * */
    void preferenceAvoidCongestion(int source);
    /**
     * 少收费
     * @param source 0:从设置页进入，1：其他
     * */
    void preferenceLessCharge(int source);
    /**
     * 不走高速
     * @param source 0:从设置页进入，1：其他
     * */
    void preferenceNotHighway(int source);
    /**
     * 高速优先
     * @param source 0:从设置页进入，1：其他
     * */
    void preferenceFirstHighway(int source);
    /**
     * 大路有限
     * @param source 0:从设置页进入，1：其他
     * */
    void preferenceFirstMainRoad(int source);
    /**
     * 时间最短
     * @param source 0:从设置页进入，1：其他
     * */
    void preferenceFastestSpeed(int source);

    /**
     * 关闭场景
     */
    void closeScene();
}
