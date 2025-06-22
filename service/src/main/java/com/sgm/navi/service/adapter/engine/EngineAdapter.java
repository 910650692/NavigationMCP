package com.sgm.navi.service.adapter.engine;


import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.define.engine.GaodeLogLevel;
import com.sgm.navi.service.define.map.MapType;

/**
 * TODO说明
 *
 * @author lvww
 * @version $Revision.2024/11/24$
 */
public final class EngineAdapter {
    private static final String ENGINE_API_PKG = EngineAdapter.class.getPackage().getName();
    private static final String ENGINE_API_CLS = "EngineAdapterImpl";
    private final IEngineApi mIEngineApi;

    private EngineAdapter() {
        mIEngineApi = (IEngineApi) AdapterConfig.getObject(ENGINE_API_PKG, ENGINE_API_CLS);
    }

    /**
     * 添加初始化引擎观察者
     *
     * @param observer EngineObserver
     */
    public void addEngineObserver(final EngineObserver observer) {
        mIEngineApi.addInitEnginObserver(observer);
    }

    public void checkSdkLimit() {
        mIEngineApi.checkSdkLimit();
    }

    public void initBaseLibs() {
        mIEngineApi.initBaseLibs();
    }

    public void initBL() {
        mIEngineApi.initBL();
    }

    /**
     * 获取引擎id
     *
     * @param mapId mapId
     * @return int mapId
     */
    public int engineID(final MapType mapId) {
        return mIEngineApi.engineID(mapId);
    }

    /**
     * 反初始化引擎
     */
    public void unInitEngine() {
        mIEngineApi.unInit();
    }

    public String getSdkVersion() {
        return mIEngineApi.getSdkVersion();
    }

    public String getEngineVersion() {
        return mIEngineApi.getEngineVersion();
    }

    public static EngineAdapter getInstance() {
        return Helper.EA;
    }

    /**
     * 获取鹰眼引擎id
     *
     * @param mapId MapType
     * @return 鹰眼引擎id
     */
    public int eagleEyeEngineID(final MapType mapId) {
        return mIEngineApi.eagleEyeEngineID(mapId);
    }

    /**
     * 获取EGLDeviceID
     *
     * @param mapId MapType
     * @return EGLDeviceID
     */
    public int mapDeviceID(final MapType mapId) {
        return mIEngineApi.mapDeviceID(mapId);
    }

    /**
     * 获取style_bl路径
     *
     * @param mapTypeId MapType
     * @return 路径字符串
     */
    public String styleBlPath(final MapType mapTypeId) {
        return mIEngineApi.styleBlPath(mapTypeId);
    }

    /**
     * 切换log级别
     *
     * @param logLevel logLevel
     */
    public void switchLog(final GaodeLogLevel logLevel) {
        mIEngineApi.switchLog(logLevel);
    }

    public String getChanelName() {
        return mIEngineApi.getChanelName();
    }

    private static final class Helper {
        private static final EngineAdapter EA = new EngineAdapter();
    }
}
