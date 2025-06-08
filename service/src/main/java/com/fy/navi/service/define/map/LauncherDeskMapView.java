package com.fy.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class LauncherDeskMapView extends  FullScreenMapView {

    public LauncherDeskMapView(@NonNull Context context) {
        super(context);
    }

    public LauncherDeskMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public LauncherDeskMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public MapType provideMapTypeId() {
        return MapType.LAUNCHER_DESK_MAP;
    }
}
