package com.fy.navi.service.logicpaket.engine;

import com.android.utils.ConvertUtils;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.adapter.engine.EngineObserver;
import com.fy.navi.service.define.engine.GaodeLogLevel;
import com.fy.navi.service.define.map.MapType;

import java.util.Hashtable;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class EnginePackage implements EngineObserver {
    private final EngineAdapter mEngineAdapter;
    private final Hashtable<String, IEngineObserver> engineObserverHashtable;

    private EnginePackage() {
        engineObserverHashtable = new Hashtable<>();
        mEngineAdapter = EngineAdapter.getInstance();
        mEngineAdapter.addEngineObserver(this);
    }

    public void addEngineObserver(String key, IEngineObserver observer) {
        engineObserverHashtable.put(key, observer);
    }

    /**
     * 初始化BaseLibs
     */
    public void initBaseLibs() {
        mEngineAdapter.initBaseLibs();
    }

    /**
     * 初始化BL
     */
    public void initBL() {
        mEngineAdapter.initBL();
    }

    public int getEngineID(MapType mapId) {
        return mEngineAdapter.engineID(mapId);
    }

    public int getEagleEyeEngineID(MapType mapId) {
        return mEngineAdapter.eagleEyeEngineID(mapId);
    }

    public boolean engineStatus() {
        return mEngineAdapter.engineStatus();
    }

    public void unInitEngine() {
        mEngineAdapter.unInitEngine();
    }

    public String getSdkVersion() {
        return mEngineAdapter.getSdkVersion();
    }

    public String getEngineVersion() {
        return mEngineAdapter.getEngineVersion();
    }

    public static EnginePackage getInstance() {
        return Helper.ep;
    }

    public void switchLog(GaodeLogLevel logLevel){
        mEngineAdapter.switchLog(logLevel);
    }

    @Override
    public void onInitEngineSuccess() {
        for (IEngineObserver iEngineObserver : engineObserverHashtable.values()) {
            if (ConvertUtils.isEmpty(iEngineObserver)) continue;
            iEngineObserver.onInitEngineSuccess();
        }
    }

    @Override
    public void onInitEngineFail(int code, String msg) {
        for (IEngineObserver iEngineObserver : engineObserverHashtable.values()) {
            if (ConvertUtils.isEmpty(iEngineObserver)) continue;
            iEngineObserver.onInitEngineFail(code, msg);
        }
    }

    private static final class Helper {
        private static final EnginePackage ep = new EnginePackage();
    }
}
