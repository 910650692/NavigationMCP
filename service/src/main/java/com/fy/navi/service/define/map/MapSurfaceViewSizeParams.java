package com.fy.navi.service.define.map;

import com.fy.navi.service.AppContext;

public class MapSurfaceViewSizeParams {
    public long x;
    public long y;
    public long width;
    public long height;
    public long screenWidth;
    public long screenHeight;
    public int densityDpi;
    /*** 底图左侧偏移量，偏移值为打开的fragment宽度，全览时会用该参数 **/
    public int screenLeftOffset;

    public MapSurfaceViewSizeParams() {
        this.x = 0;
        this.y = 0;
        this.width = AppContext.getInstance().getMContext().getResources().getDisplayMetrics().widthPixels;
        this.height = AppContext.getInstance().getMContext().getResources().getDisplayMetrics().heightPixels;
        this.screenWidth = AppContext.getInstance().getMContext().getResources().getDisplayMetrics().widthPixels;
        this.screenHeight = AppContext.getInstance().getMContext().getResources().getDisplayMetrics().heightPixels;
        this.densityDpi = AppContext.getInstance().getMContext().getResources().getDisplayMetrics().densityDpi;
    }

    public MapSurfaceViewSizeParams(long x, long y, long width, long height, long screenWidth, long screenHeight, int densityDpi) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.screenWidth = screenWidth;
        this.screenHeight = screenHeight;
        this.densityDpi = densityDpi;
    }
}
