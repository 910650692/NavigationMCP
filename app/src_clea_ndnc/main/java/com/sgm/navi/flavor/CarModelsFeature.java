package com.sgm.navi.flavor;

import static com.sgm.navi.service.MapDefaultFinalTag.NAVI_EXIT;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.launcher.LauncherWindowService;
import com.sgm.navi.hmi.splitscreen.SplitScreenManager;
import com.sgm.navi.service.MapDefaultFinalTag;

/**
 * @Description TODO
 * @Author lvww
 * @date 2025/1/2
 */
public class CarModelsFeature extends BaseCarModelsFeature {
    public CarModelsFeature() {
        super();
        Logger.i(MapDefaultFinalTag.DEFAULT_TAG, "Flavor type", "我是NdNc车型设备");
    }

    @Override
    public void initComponent() {
        SplitScreenManager.getInstance().init();
        LauncherWindowService.startService();
    }

    @Override
    public void exitApp() {
        if (Logger.openLog) {
            Logger.printStackTrace(NAVI_EXIT,true);
        }
        System.exit(0);
    }

    public static CarModelsFeature getInstance() {
        return Helper.cmf;
    }

    private static final class Helper {
        private static final CarModelsFeature cmf = new CarModelsFeature();
    }
}
