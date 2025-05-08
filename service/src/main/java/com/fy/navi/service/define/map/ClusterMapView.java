package com.fy.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

/**
 * 仪表MapView
 */
public class ClusterMapView extends  FullScreenMapView implements IBaseScreenMapView {

    public ClusterMapView(@NonNull Context context) {
        super(context);
    }

    public ClusterMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ClusterMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public MapType provideMapTypeId() {
        return MapType.CLUSTER_MAP;
    }
}
