package com.sgm.navi.service.define.map;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.sgm.navi.service.utils.HudMapConfigUtil;

public class HUDMapView extends FullScreenMapView {
    private boolean mOpenScreenStatus = true;
    private boolean mIsBindView = false;

    public HUDMapView(@NonNull Context context) {
        super(context);
    }

    public HUDMapView(@NonNull Context context, @NonNull AttributeSet attrs) {
        super(context, attrs);
    }
    public HUDMapView(@NonNull Context context, @NonNull AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
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

    @Override
    public long getMapViewWidth() {
        return HudMapConfigUtil.getInstance().getHudMapWidth();
    }

    @Override
    public long getMapViewHeight() {
        return HudMapConfigUtil.getInstance().getHudMapHeight();
    }

    @Override
    public long getScreenWidth() {
        return HudMapConfigUtil.getInstance().getHudMapWidth();
    }

    @Override
    public long getScreenHeight() {
        return HudMapConfigUtil.getInstance().getHudMapHeight();
    }

    @Override
    public boolean isOpenScreen() {
        Logger.d("HUDMapView", "isOpenScreen", "mOpenScreenStatus", mOpenScreenStatus);
        return mOpenScreenStatus;
    }

    @Override
    public boolean isBindMapView() {
        return mIsBindView;
    }
}
