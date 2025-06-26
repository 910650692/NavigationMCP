package com.sgm.navi.flavor;

import com.android.utils.log.Logger;
import com.sgm.navi.service.MapDefaultFinalTag;

/**
 * @Description TODO
 * @Author lvww
 * @date 2025/1/2
 */
public class CarModelsFeature extends BaseCarModelsFeature {
    public CarModelsFeature() {
        super();
        Logger.i(MapDefaultFinalTag.DEFAULT_TAG, "Flavor type", "我是别克车型设备");
    }

    @Override
    public void initComponent() {

    }

    @Override
    public void exitApp() {
        System.exit(0);
    }

    public static CarModelsFeature getInstance() {
        return Helper.cmf;
    }

    private static final class Helper {
        private static final CarModelsFeature cmf = new CarModelsFeature();
    }
}
