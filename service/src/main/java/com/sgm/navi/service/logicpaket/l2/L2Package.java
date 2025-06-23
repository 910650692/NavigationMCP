package com.sgm.navi.service.logicpaket.l2;

import com.android.utils.log.Logger;
import com.sgm.navi.service.adapter.l2.L2Adapter;
import com.sgm.navi.service.adapter.l2.L2DriveObserver;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteOdd;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.L2NaviBean;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
public class L2Package {
    private static final String TAG = L2Package.class.getSimpleName();

    private final L2Adapter mL2Adapter;
    private final LayerAdapter mLayerAdapter;
    private boolean mInitialized = false;

    //region INSTANCE
    public static L2Package getInstance() {
        return SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final L2Package INSTANCE = new L2Package();
    }

    private L2Package() {
        mL2Adapter = L2Adapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
    }
    //endregion

    public void init() {
        int type = CalibrationPackage.getInstance().adasConfigurationType();
        if (type != 9 && type != 8) {
            Logger.i(TAG, "not L2++ configuration");
            return;
        }
        if (mInitialized) {
            Logger.i(TAG, "initialized");
            return;
        }
        mL2Adapter.registerCallback(mL2DriveObserver);
        mL2Adapter.init();
        mInitialized = true;
    }

    private final L2DriveObserver mL2DriveObserver = new L2DriveObserver() {
        @Override
        public void onSdTbtDataChange(L2NaviBean l2NaviBean) {
            for (L2InfoCallback callback : mCallbacks.values()) {
                callback.onSdTbtDataChange(l2NaviBean);
            }
        }
    };

    //region 回调管理
    private final ConcurrentHashMap<String, L2InfoCallback> mCallbacks = new ConcurrentHashMap<>();

    public void registerCallback(String key, L2InfoCallback callback) {
        Logger.d(TAG, "addCallback", key);
        mCallbacks.put(key, callback);
    }

    public void unregisterCallback(String key) {
        Logger.d(TAG, "removeCallback", key);
        mCallbacks.remove(key);
    }
    //endregion

    /* 更新Odd信息 */
    public void updateOddInfo(MapType mapTypeId, ArrayList<LayerItemRouteOdd> oddInfoList, long pathId) {
        mLayerAdapter.updateOddInfo(mapTypeId, oddInfoList, pathId);
    }

    public int getLinkDist(long curSegIdx, long curLinkIdx) {
        return mL2Adapter.getLinkDist(curSegIdx, curLinkIdx);
    }
}