package com.fy.navi.service.logicpaket.l2;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.adapter.l2.L2Adapter;
import com.fy.navi.service.adapter.l2.L2DriveObserver;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
public class L2Package implements L2DriveObserver {
    private final ConcurrentMap<String, L2InfoCallback> l2InfoCallbackMap = new ConcurrentHashMap<>();
    private L2Adapter mL2Adapter;

    private L2Package() {
        mL2Adapter = L2Adapter.getInstance();
        mL2Adapter.registerCallback(this);
    }

    public static L2Package getInstance() {
        return Helper.l2Package;
    }

    public void registerCallback(String packageName, L2InfoCallback l2InfoCallback) {
        Logger.i("lvww", "registerCallback");
        Map<String, L2InfoCallback> map = ConvertUtils.push(l2InfoCallbackMap, packageName, l2InfoCallback);
        Logger.i("lvww", "map" + l2InfoCallbackMap.size());
    }

    public void unregisterCallback(String packageName) {
        ConvertUtils.remove(l2InfoCallbackMap, packageName);
    }

    /*public void setEndParkInfo(double enterX, double enterY, double exitX, double exitY) {
        mL2Adapter.setEndParkInfo(enterX, enterY, exitX, exitY);
    }

    public void startPeriodicTask(){
        mL2Adapter.startPeriodicTask();
    }

    public void stopPeriodicTask(){
        mL2Adapter.stopPeriodicTask();
    }*/

    @Override
    public void onSdTbtDataChange(String json) {
        for (L2InfoCallback callback : l2InfoCallbackMap.values()) {
            callback.onSdTbtDataChange(json);
        }
    }

    private static final class Helper {
        public static final L2Package l2Package = new L2Package();
    }
}