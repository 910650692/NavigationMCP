package com.sgm.navi.service.define.map;

import com.android.utils.ScreenUtils;
import com.sgm.navi.service.AppCache;

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
    private boolean isOpenScreen;
    /*** 底图左侧偏移量，偏移值为打开的fragment宽度，全览时会用该参数 **/
    private int screenLeftOffset;

    public MapViewParams() {
        this.x = 0;
        this.y = 0;
        this.screenWidth = ScreenUtils.Companion.getInstance().getRealScreenWidth(AppCache.getInstance().getMContext());
        this.screenHeight = ScreenUtils.Companion.getInstance().getRealScreenHeight(AppCache.getInstance().getMContext());
        this.width = screenWidth;
        this.height = screenHeight;
        this.densityDpi = AppCache.getInstance().getMContext().getResources().getDisplayMetrics().densityDpi;
        this.isOpenScreen = false;
    }

    public MapViewParams(long x, long y, long width, long height, long screenWidth, long screenHeight, int densityDpi, boolean isOpenScreen) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.screenWidth = screenWidth;
        this.screenHeight = screenHeight;
        this.densityDpi = densityDpi;
        this.isOpenScreen = isOpenScreen;
    }
}
