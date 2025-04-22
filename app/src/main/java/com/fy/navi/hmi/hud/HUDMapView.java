package com.fy.navi.hmi.hud;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.service.define.map.FullScreenMapView;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;

/**
 * HUD地图 MapView
 */

public class HUDMapView extends FullScreenMapView implements IBaseScreenMapView {
    public HUDMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public HUDMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public HUDMapView(@NonNull Context context) {
        super(context);
    }
    @Override
    public MapType provideMapTypeId() {
        return MapType.HUD_MAP;
    }
}
