package com.fy.navi.service.define.layer;

import androidx.annotation.IntDef;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/12
 */
public class CarModeType {
    /* 2D默认车标 */
    public static final int CAR_MODEL_TYPE_2D = 0;
    /* 3D默认车标 */
    public static final int CAR_MODEL_TYPE_3D = 1;
    /* 3D骨骼车标 */
    public static final int CAR_MODEL_TYPE_SKELETON = 2;
    /* 3D车速车标 */
    public static final int CAR_MODEL_TYPE_SPEED = 3;

    @IntDef({CAR_MODEL_TYPE_2D, CAR_MODEL_TYPE_3D, CAR_MODEL_TYPE_SKELETON, CAR_MODEL_TYPE_SPEED})
    @Retention(RetentionPolicy.RUNTIME)
    public @interface CarModelTypeId {

    }
}
