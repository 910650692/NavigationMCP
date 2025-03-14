package com.fy.navi.service.define.position;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class LocGNSSStatus {
    public static final int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
    public static final int LocGNSSStatusV = 86;
    public static final int LocGNSSStatusA = 65;

    public LocGNSSStatus() {
    }

    @Retention(RetentionPolicy.CLASS)
    public @interface LocGNSSStatus1 {
    }
}
