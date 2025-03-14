package com.fy.navi.service.define.position;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class LocMoveStatus {

    public static final int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
    public static final int LocMoveInvalid = 0;
    public static final int LocMoveNormal = 1;
    public static final int LocMoveBack = 2;
    public static final int LocMoveLeft = 4;
    public static final int LocMoveRight = 8;
    public static final int LocMoveTurnRound = 16;
    public static final int LocMoveStop = 32;

    public LocMoveStatus() {
    }

    @Retention(RetentionPolicy.CLASS)
    public @interface LocMoveStatus1 {
    }
}
