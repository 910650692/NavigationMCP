package com.fy.navi.scene.api.route;

public interface ISceneRouteSelectCallBack {
    /**
     * 更新列表
     * */
    void onResultListUpdate();
    /**
     * 路线列表点击
     * @param isTheSameIndex   是否点击已选中的路线
     * @param index   点击索引
     * */
    void onRouteSelect(boolean isTheSameIndex, int index);
    /**
     * touch
     * */
    void onListTouch();

}
