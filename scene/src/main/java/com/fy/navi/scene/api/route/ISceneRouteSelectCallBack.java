package com.fy.navi.scene.api.route;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public interface ISceneRouteSelectCallBack {

    void onResultListUpdate();

    void onRouteSelect(boolean isTheSameIndex, int index);

    void onListTouch();

}
