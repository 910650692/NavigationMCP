package com.fy.navi.service.logicpaket.l2;

import com.android.utils.ConvertUtils;
import com.fy.navi.service.adapter.l2.L2Adapter;
import com.fy.navi.service.adapter.l2.L2DriveObserver;
import com.fy.navi.service.define.navi.L2NaviBean;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
public class L2Package implements L2DriveObserver {
    private final ConcurrentMap<String, L2InfoCallback> l2InfoCallbackMap = new ConcurrentHashMap<>();

    private L2Package() {
        L2Adapter.getInstance().registerCallback(this);
    }

    public static L2Package getInstance() {
        return Helper.l2Package;
    }

    public void registerCallback(String packageName, L2InfoCallback l2InfoCallback) {
        ConvertUtils.push(l2InfoCallbackMap, packageName, l2InfoCallback);
    }

    public void unregisterCallback(String packageName) {
        ConvertUtils.remove(l2InfoCallbackMap, packageName);
    }

    @Override
    public void onNaviStatus(L2NaviBean l2NaviBean) {
        for (L2InfoCallback callback : l2InfoCallbackMap.values()) {
            callback.onNaviStatus(l2NaviBean);
        }
    }

    @Override
    public void onSelectRouteIndex(L2NaviBean l2NaviBean) {
        for (L2InfoCallback callback : l2InfoCallbackMap.values()) {
            callback.onSelectRouteIndex(l2NaviBean);
        }
    }

    private static final class Helper {
        public static final L2Package l2Package = new L2Package();
    }
}