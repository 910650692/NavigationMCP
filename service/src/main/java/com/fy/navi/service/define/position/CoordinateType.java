package com.fy.navi.service.define.position;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class CoordinateType {

    public static final int BD09LL = 0;
    public static final int BD09MC = 1;
    public static final int GCJ02LL = 2;
    public static final int WGS84LL = 3;

    public CoordinateType() {
    }

    @Retention(RetentionPolicy.CLASS)
    public @interface BDCoordinateType {
    }
}
