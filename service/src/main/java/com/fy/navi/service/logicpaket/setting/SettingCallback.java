package com.fy.navi.service.logicpaket.setting;

public interface SettingCallback {
    /**
     * @param eventType 同步SDK回调事件类型
     * @param exCode 同步SDK返回值
     */
    void notify(int eventType, int exCode);

    /**
     * 设置项发生变化回调
     * @param key 设置项
     * @param value 设置项值
     */
    default void onSettingChanged(String key, String value){

    }
}
