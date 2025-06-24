package com.sgm.navi.service.adapter.engine;

/**
 * TODO 说明
 * @author lvww
 * @version $Revision.2024/11/26$
 */
public interface EngineObserver {

    void onLoadLibraryFail(int code, String msg);

    void onLoadLibrarySuccess();

    default void onInitBaseLibSuccess(){

    }

    /**
     * 引擎初始化成功回调
     */
    void onInitEngineSuccess();

    /**
     * 引擎初始化失败回调
     * @param code 错误码
     * @param msg 错误信息
     */
    void onInitEngineFail(int code, String msg);
}
