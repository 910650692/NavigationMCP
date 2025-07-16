package com.sgm.navi.flavor;

import android.content.Context;
import android.content.IntentFilter;
import android.util.Log;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.DeviceUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.broadcast.PoiPushReceiver;
import com.sgm.navi.broadcast.SteeringWheelButtonReceiver;
import com.sgm.navi.burypoint.BuryManager;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.service.AppCache;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @Description TODO
 * @Author lvww
 * @date 2025/1/2
 */
public abstract class BaseCarModelsFeature {

    private boolean mHasRegister;
    private SteeringWheelButtonReceiver mWheelButtonReceiver;
    private PoiPushReceiver mPoiPushReceiver;

    BaseCarModelsFeature() {
        initPublicComponent();
    }

    private void initPublicComponent() {
        initARouter();
        initDataTrack();
        initBroadcast();
    }

    /**
     * 初始化路由
     */
    private void initARouter() {
        if (BuildConfig.DEBUG) {
            ARouter.openLog();
            ARouter.openDebug();
        }
        ARouter.init(AppCache.getInstance().getMApplication());
    }

    /**
     * 初始化埋点SDK
     */
    private void initDataTrack() {
        BuryManager.getInstance().initPatacDataTrackManager(AppCache.getInstance().getMApplication(),
                DeviceUtils.isCar(AppCache.getInstance().getMApplication()));
    }

    private static void createDimens(final int dpi, final String unit, final int size) {
        double defaultDensity = 160.0;
        double density = dpi/defaultDensity;
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < size; i++) {
            BigDecimal bigDecimal = new BigDecimal(i / density);
            BigDecimal rounded = bigDecimal.setScale(2, RoundingMode.HALF_UP).stripTrailingZeros();
            stringBuilder.append("<dimen name=")
                    .append(unit)
                    .append("_")
                    .append(i)
                    .append("\">")
                    .append(rounded)
                    .append(unit)
                    .append("</dimen>")
                    .append("\n");
        }
        Log.e("lvww", stringBuilder.toString());
    }

    private void initBroadcast() {
        if (mHasRegister) {
            return;
        }
        mWheelButtonReceiver = new SteeringWheelButtonReceiver();
        IntentFilter intentFilter = new IntentFilter(SteeringWheelButtonReceiver.ACTION);
        AppCache.getInstance().getMContext().registerReceiver(mWheelButtonReceiver, intentFilter, Context.RECEIVER_EXPORTED);

        mPoiPushReceiver = new PoiPushReceiver();
        IntentFilter poiFilter = new IntentFilter(PoiPushReceiver.PUSH_ACTION);
        AppCache.getInstance().getMContext().registerReceiver(mPoiPushReceiver, poiFilter, Context.RECEIVER_EXPORTED);
        mHasRegister = true;
    }

    public void unRegisterBroadcast() {
        if (null != AppCache.getInstance().getMContext() && mHasRegister) {
            if (null != mWheelButtonReceiver) {
                AppCache.getInstance().getMContext().unregisterReceiver(mWheelButtonReceiver);
                mWheelButtonReceiver = null;
            }
            if (null != mPoiPushReceiver) {
                AppCache.getInstance().getMContext().unregisterReceiver(mPoiPushReceiver);
                mPoiPushReceiver = null;
            }
            mHasRegister = false;
        }
    }

    /**
     * 特殊车型组件初始化，可以是耗时操作
     */
    public abstract void initComponent();

    public abstract void exitApp();
}
