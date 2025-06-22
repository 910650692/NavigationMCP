package com.sgm.navi.service.logicpaket.engine;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public interface IEngineObserver {
    default void onInitBaseLibSuccess(){

    }

    void onInitEngineSuccess();

    void onInitEngineFail(int code, String msg);
}
