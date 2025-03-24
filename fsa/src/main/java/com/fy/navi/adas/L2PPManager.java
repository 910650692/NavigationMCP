package com.fy.navi.adas;

import com.fy.navi.service.logicpaket.l2.L2InfoCallback;
import com.fy.navi.service.logicpaket.l2.L2Package;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.DataType;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public final class L2PPManager {
    private static final String TAG = L2PPManager.class.getSimpleName();

    private AdasManager mAdasManager;
    private String mTBTJson;
    private ScheduledExecutorService mScheduler;

    public static L2PPManager getInstance() {
        return SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final L2PPManager INSTANCE = new L2PPManager();
    }

    private L2PPManager() {
    }

    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onL2DataCallBack(final String json) {
            if (json == null || mAdasManager == null) {
                return;
            }
            mAdasManager.sendData(DataType.SDRoute, json.getBytes());
        }
    };

    private final L2InfoCallback mL2InfoCallback = new L2InfoCallback() {
        @Override
        public void onNaviStatus(final String json) {
            mTBTJson = json;
        }

        @Override
        public void onSelectRouteIndex(final String json) {
            mTBTJson = json;
        }

        @Override
        public void onParkingInfo(final String json) {
            mTBTJson = json;
        }

        @Override
        public void onNaviInfo(final String json) {
            mTBTJson = json;
        }
    };

    /**
     * 初始化
     *
     * @param adasManager
     */
    public void init(final AdasManager adasManager) {
        mAdasManager = adasManager;
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        L2Package.getInstance().registerCallback(TAG, mL2InfoCallback);
        mScheduler = Executors.newScheduledThreadPool(1);
        mScheduler.scheduleWithFixedDelay(mTask, 0, 1, TimeUnit.SECONDS);
    }

    private final Runnable mTask = new Runnable() {
        @Override
        public void run() {
            if (mTBTJson == null || mAdasManager == null) {
                return;
            }
            mAdasManager.sendData(DataType.SDPeriodShortData, mTBTJson.getBytes());
        }
    };
}
