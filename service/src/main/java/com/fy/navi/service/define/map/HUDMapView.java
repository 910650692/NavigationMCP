package com.fy.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class HUDMapView extends FullScreenMapView implements IBaseScreenMapView {

//    // 单例实例
//    private static final HUDMapView INSTANCE = new HUDMapView(AppContext.getInstance().getMContext());

    public HUDMapView(@NonNull Context context) {
        super(context);
    }
//    /**
//     * 获取单例实例
//     *
//     * @return HUDMapView的单例对象
//     */
//    public static HUDMapView getInstance() {
//        return INSTANCE;
//    }

    @Override
    public MapType provideMapTypeId() {
        return MapType.HUD_MAP;
    }
    public HUDMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public HUDMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

//    @Override
//    public long getMapViewWidth() {
//        Logger.i("getMapViewWidth"+ScreenUtils.Companion.getInstance().getScreenWidth());
//        return ScreenUtils.Companion.getInstance().getScreenWidth();
//    }
//    @Override
//    public long getMapViewHeight() {
//        Logger.i("getMapViewHeight"+ScreenUtils.Companion.getInstance().getScreenHeight());
//        return ScreenUtils.Companion.getInstance().getScreenHeight();
//    }
}
