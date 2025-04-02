package com.fy.navi.service.adapter.engine;

import androidx.work.ListenableWorker;

import com.fy.navi.service.define.map.MapType;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public interface IEngineApi {


    void addInitEnginObserver(EngineObserver observer);

    ListenableWorker.Result initEngine();

    int engineID(MapType mapId);

    int eagleEyeEngineID(MapType mapId);

    int mapDeviceID(MapType mapId);

    void switchLog();

    boolean engineStatus();

    void unInit();

    String getSdkVersion();

    String getEngineVersion();

    String styleBlPath(MapType mapTypeId);
}
