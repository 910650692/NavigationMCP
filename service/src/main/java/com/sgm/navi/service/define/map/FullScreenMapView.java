package com.sgm.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.MapDefaultFinalTag;

public abstract class FullScreenMapView extends FrameLayout implements IBaseScreenMapView {
    private MapType mapTypeId = MapType.MAIN_SCREEN_MAIN_MAP;

    public FullScreenMapView(@NonNull Context context) {
        super(context);
    }

    public FullScreenMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public FullScreenMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public Context getMapViewContext() {
        return getContext();
    }

    @Override
    public long getMapViewHeight() {
        return getScreenHeight();
    }

    @Override
    public long getMapViewWidth() {
        return getScreenWidth();
    }

    @Override
    public long getScreenHeight() {
        return ScreenUtils.Companion.getInstance().getRealScreenHeight(getMapViewContext());
    }

    @Override
    public long getScreenWidth() {
        return ScreenUtils.Companion.getInstance().getRealScreenWidth(getMapViewContext());
    }

    @Override
    public int getScreenDensityDpi() {
        return ScreenUtils.Companion.getInstance().densityDpi();
    }

    @Override
    public long getMapViewX() {
        return 0;
    }

    @Override
    public long getMapViewY() {
        return 0;
    }

    @Override
    public void bindMapView(View mapSurfaceView) {
        ViewParent viewParent = mapSurfaceView.getParent();
        if (viewParent != null) {
            ViewGroup viewGroup = (ViewGroup) viewParent;
            viewGroup.removeView(mapSurfaceView);
        }
        if (isBindMapView()) {
            addView(mapSurfaceView);
            Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, provideMapTypeId(), " bindMapView");
        } else {
            Logger.e(MapDefaultFinalTag.MAP_SERVICE_TAG, provideMapTypeId(), " not bindMapView");
        }
    }

    @Override
    public void unBindMapView(View mapSurfaceView) {
        removeView(mapSurfaceView);
        Logger.e(MapDefaultFinalTag.MAP_SERVICE_TAG, provideMapTypeId(), " unBindMapView");
    }

    @Override
    public MapType provideMapTypeId() {
        return mapTypeId;
    }
}
