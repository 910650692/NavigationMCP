package com.sgm.navi.service.adapter.position;


import androidx.annotation.IntDef;

import com.android.utils.DeviceUtils;
import com.sgm.navi.service.AppCache;

public interface PositionConstant {
    /*** 是否为后端融合 **/
    boolean isDrBack = DeviceUtils.isCar(AppCache.getInstance().getMApplication());

    interface GearType {
        int GEAR_NEUTRAL = 1;//空挡
        int GEAR_REVERSE = 3;//倒挡
        int GEAR_PARK = 0;//停车挡
        int GEAR_DRIVE = 2;//行驶挡
        int GEAR_INVALID = 4;//无效
    }

    @IntDef({DRDebugEvent.DR_TYPE_GPS, DRDebugEvent.DR_TYPE_SENSOR,
            DRDebugEvent.DR_TYPE_SPEED, DRDebugEvent.DR_LOSS_RATE})
    @interface DRDebugEvent {
        int DR_TYPE_GPS = 1;
        int DR_TYPE_SENSOR = 2;
        int DR_TYPE_SPEED = 3;
        int DR_LOSS_RATE = 4;
    }
}
