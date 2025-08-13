package com.sgm.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

/**
 * 后排屏mapview
 */
public class RearScreenMapView extends FullScreenMapView {

    public RearScreenMapView(@NonNull Context context) {
        super(context);
    }

    public RearScreenMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public RearScreenMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public MapType provideMapTypeId() {
        return MapType.REAR_SCREEN_MAP;
    }

}
