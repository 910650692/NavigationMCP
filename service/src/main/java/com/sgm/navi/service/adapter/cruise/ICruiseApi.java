package com.sgm.navi.service.adapter.cruise;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface ICruiseApi {

    void initCruise();

    void registerObserver(String key, CruiseObserver guidanceObserver);

    boolean startCruise();

    boolean stopCruise();

    void unregisterObserver(String key);

    void unInitCruise();
}
