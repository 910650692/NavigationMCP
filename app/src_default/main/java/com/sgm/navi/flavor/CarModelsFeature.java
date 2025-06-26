package com.sgm.navi.flavor;

import com.android.utils.crash.AppCrashRecord;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;

/**
 * @Description TODO
 * @Author lvww
 * @date 2025/1/2
 */
public class CarModelsFeature extends BaseCarModelsFeature {
    private CarModelsFeature() {
        super();
        Logger.i(MapDefaultFinalTag.DEFAULT_TAG, "Flavor type", "我是平板车型设备");
    }

    @Override
    public void initComponent() {
        Thread.setDefaultUncaughtExceptionHandler(new AppCrashRecord(AppCache.getInstance().getMContext()));
    }

    @Override
    public void exitApp() {

    }

    public static CarModelsFeature getInstance() {
        return Helper.cmf;
    }

    private static final class Helper {
        private static final CarModelsFeature cmf = new CarModelsFeature();
    }
}
