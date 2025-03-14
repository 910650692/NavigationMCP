package com.fy.navi.service.adapter.engine;

import androidx.work.ListenableWorker;

import com.fy.navi.service.define.map.MapTypeId;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public interface IEngineApi {


    void addInitEnginObserver(EngineObserver observer);

    ListenableWorker.Result initEngine();

    int engineID(MapTypeId mapId);

    void switchLog();

    boolean engineStatus();

    void unInit();

    int eagleEyeEngineID(MapTypeId mapId);

    String getSdkVersion();

    String getEngineVersion();
}
