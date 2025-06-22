package com.sgm.navi.scene.api.route;

public interface ISceneRouteDetailsSelectCallBack {
    /**
     * 避开道路回调
     * @param routeAvoidInfo 是否避开状态
     * */
    void onRouteDetailsChecked(boolean routeAvoidInfo);

}
