package com.fy.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

// 主要是给Launcher 桌面上的Widget使用
public class CardWidgetMapView extends FullScreenMapView {

    public CardWidgetMapView(@NonNull Context context) {
        super(context);
    }

    public CardWidgetMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public CardWidgetMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public MapType provideMapTypeId() {
        return MapType.LAUNCHER_WIDGET_MAP;
    }

    @Override
    public long getScreenWidth() {
        return getRight();
    }

    @Override
    public long getScreenHeight() {
        return getBottom();
    }

    @Override
    public long getMapViewWidth() {
        return getRight();
    }

    @Override
    public long getMapViewHeight() {
        return getBottom();
    }
}
