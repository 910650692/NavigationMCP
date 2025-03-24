package com.fy.navi.scene.api.route;

public interface ISceneRoutePreferenceCallBack {
    /**
     * 偏好回调
     * @param text  文本
     * @param isFirstChange 是否第一次
     * */
    void onRoutePreferenceChange(String text, boolean isFirstChange);

}
