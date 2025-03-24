package com.fy.navi.service.adapter.setting;

public interface SettingAdapterCallback {
    /**
     * @param eventType 同步SDK回调事件类型
     * @param exCode 同步SDK返回值
     */
    void notify(int eventType, int exCode);
}