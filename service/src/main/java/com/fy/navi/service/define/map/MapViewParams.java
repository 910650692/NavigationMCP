package com.fy.navi.service.define.map;

import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MapViewParams {
    private long x;
    private long y;
    private long width;
    private long height;
    private long screenWidth;
    private long screenHeight;
    private int densityDpi;
    /*** 底图左侧偏移量，偏移值为打开的fragment宽度，全览时会用该参数 **/
    private int screenLeftOffset;

    public MapViewParams() {
        this.x = 0;
        this.y = 0;
        this.screenWidth = AppContext.getInstance().getMContext().getResources().getDisplayMetrics().widthPixels;
        this.screenHeight = AppContext.getInstance().getMContext().getResources().getDisplayMetrics().heightPixels;
        this.width = screenWidth;
        this.height = screenHeight;
        this.densityDpi = AppContext.getInstance().getMContext().getResources().getDisplayMetrics().densityDpi;
    }

    public MapViewParams(long x, long y, long width, long height, long screenWidth, long screenHeight, int densityDpi) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.screenWidth = screenWidth;
        this.screenHeight = screenHeight;
        this.densityDpi = densityDpi;
    }
}
