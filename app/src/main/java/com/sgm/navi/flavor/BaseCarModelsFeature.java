package com.sgm.navi.flavor;

import android.util.Log;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.DeviceUtils;
import com.android.utils.thread.ThreadManager;
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
    BaseCarModelsFeature() {
        initPublicComponent();
    }

    private void initPublicComponent() {
        initARouter();
        initDataTrack();
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

    private void createDimens(int dpi) {
        double density = 1.25;
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < 4000; i++) {
            BigDecimal bigDecimal = new BigDecimal(i / density);
            BigDecimal rounded = bigDecimal.setScale(2, RoundingMode.HALF_UP).stripTrailingZeros();
            stringBuilder.append("<dimen name=\"dp_")
                    .append(i)
                    .append("\">")
                    .append(rounded)
                    .append("dp")
                    .append("</dimen>")
                    .append("\n");
        }
        Log.e("lvww", stringBuilder.toString());
    }

    /**
     * 特殊车型组件初始化，可以是耗时操作
     */
    public abstract void initComponent();

    public abstract void exitApp();
}
