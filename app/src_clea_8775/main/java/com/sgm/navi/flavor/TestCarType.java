package com.sgm.navi.flavor;

import com.android.utils.log.Logger;
import com.sgm.navi.service.MapDefaultFinalTag;

/**
 * @Description TODO
 * @Author lvww
 * @date 2025/1/2
 */
public class TestCarType extends BaseTestCarType {
    public TestCarType() {
        Logger.i("lvww", "我是src_clea_87755型设备");
        Logger.i(MapDefaultFinalTag.DEFAULT_TAG, "Flavor type", "我是clea_87755车型设备");
    }
}
