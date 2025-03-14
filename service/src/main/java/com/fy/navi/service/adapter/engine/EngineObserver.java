package com.fy.navi.service.adapter.engine;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public interface EngineObserver {

    void onInitEngineSuccess();

    void onInitEngineFail(int code, String msg);
}
