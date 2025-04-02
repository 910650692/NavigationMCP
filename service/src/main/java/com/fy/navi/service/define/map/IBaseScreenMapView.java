package com.fy.navi.service.define.map;


import android.content.Context;
import android.view.View;

public interface IBaseScreenMapView {

    MapType provideMapTypeId();

    Context getMapViewContext();

    void bindMapView(View mapSurfaceView);

    void unBindMapView(View mapSurfaceView);

    long getMapViewX();

    long getMapViewY();

    long getMapViewWidth();

    long getMapViewHeight();

    long getScreenWidth();

    long getScreenHeight();

    int getScreenDensityDpi();
}
