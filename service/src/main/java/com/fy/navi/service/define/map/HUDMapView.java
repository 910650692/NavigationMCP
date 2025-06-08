package com.fy.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class HUDMapView extends FullScreenMapView {
    private boolean mOpenScreenStatus = true;
    private boolean mIsBindView = false;

    public HUDMapView(@NonNull Context context) {
        super(context);
    }

    public void setOpenScreen(boolean openScreenStatus){
        this.mOpenScreenStatus = openScreenStatus;
    }

    public void setIsBindView(boolean mBindView) {
        this.mIsBindView = mBindView;
    }

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

    @Override
    public long getMapViewWidth() {
        return 328;
    }

    @Override
    public long getMapViewHeight() {
        return 172;
    }

    @Override
    public long getScreenWidth() {
        return 328;
    }

    @Override
    public long getScreenHeight() {
        return 172;
    }

    @Override
    public boolean isOpenScreen() {
        return mOpenScreenStatus;
    }

    @Override
    public boolean isBindMapView() {
        return mIsBindView;
    }
}
