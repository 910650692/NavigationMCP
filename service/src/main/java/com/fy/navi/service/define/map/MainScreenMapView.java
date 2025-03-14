package com.fy.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class MainScreenMapView extends FullScreenMapView implements IBaseScreenMapView {

    public MainScreenMapView(@NonNull Context context) {
        super(context);
    }

    public MainScreenMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public MainScreenMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public MapTypeId provideMapTypeId() {
        return MapTypeId.MAIN_SCREEN_MAIN_MAP;
    }
}