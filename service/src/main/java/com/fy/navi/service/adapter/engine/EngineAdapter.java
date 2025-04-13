package com.fy.navi.service.adapter.engine;

import androidx.work.ListenableWorker;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.engine.GaodeLogLevel;
import com.fy.navi.service.define.map.MapType;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class EngineAdapter {
    private static final String ENGINE_API_PKG = EngineAdapter.class.getPackage().getName();
    private static final String ENGINE_API_CLS = "EngineAdapterImpl";
    private final IEngineApi mIEngineApi;

    private EngineAdapter() {
        mIEngineApi = (IEngineApi) AdapterConfig.getObject(ENGINE_API_PKG, ENGINE_API_CLS);
    }


    public void addEngineObserver(EngineObserver observer) {
        mIEngineApi.addInitEnginObserver(observer);
    }

    public ListenableWorker.Result initEngine() {
        return mIEngineApi.initEngine();
    }

    public int engineID(MapType mapId) {
        return mIEngineApi.engineID(mapId);
    }

    public boolean engineStatus() {
        return mIEngineApi.engineStatus();
    }

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
        return Helper.ea;
    }

    public int eagleEyeEngineID(MapType mapId) {
        return mIEngineApi.eagleEyeEngineID(mapId);
    }

    public int mapDeviceID(MapType mapId) {
        return mIEngineApi.mapDeviceID(mapId);
    }

    public String styleBlPath(MapType mapTypeId) {
        return mIEngineApi.styleBlPath(mapTypeId);
    }


    public void switchLog(GaodeLogLevel logLevel){
        mIEngineApi.switchLog(logLevel);
    }

    private static final class Helper {
        private static final EngineAdapter ea = new EngineAdapter();
    }
}
