package com.fy.navi.service.logicpaket.engine;

import androidx.work.ListenableWorker;

import com.android.utils.ConvertUtils;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.adapter.engine.EngineObserver;
import com.fy.navi.service.define.map.MapTypeId;

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

    public ListenableWorker.Result initEngine() {
        return mEngineAdapter.initEngine();
    }

    public int getEngineID(MapTypeId mapId) {
        return mEngineAdapter.engineID(mapId);
    }

    public int getEagleEyeEngineID(MapTypeId mapId) {
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
