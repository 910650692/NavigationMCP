package com.fy.navi.service.define.layer.bls;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/10
 */
public class RouteDrawStyles {
    public boolean mIsOffLine;
    public boolean mIsNavi;
    public int mRouteMapMode;
    public int mRouteScene;
    public boolean mIsMultipleMode;
    public int mainPathStyleType;

    public boolean ismIsOffLine() {
        return mIsOffLine;
    }

    public void setmIsOffLine(boolean mIsOffLine) {
        this.mIsOffLine = mIsOffLine;
    }

    public boolean ismIsNavi() {
        return mIsNavi;
    }

    public void setmIsNavi(boolean mIsNavi) {
        this.mIsNavi = mIsNavi;
    }

    public int getmRouteMapMode() {
        return mRouteMapMode;
    }

    public void setmRouteMapMode(int mRouteMapMode) {
        this.mRouteMapMode = mRouteMapMode;
    }

    public int getmRouteScene() {
        return mRouteScene;
    }

    public void setmRouteScene(int mRouteScene) {
        this.mRouteScene = mRouteScene;
    }

    public boolean ismIsMultipleMode() {
        return mIsMultipleMode;
    }

    public void setmIsMultipleMode(boolean mIsMultipleMode) {
        this.mIsMultipleMode = mIsMultipleMode;
    }

    public int getMainPathStyleType() {
        return mainPathStyleType;
    }

    public void setMainPathStyleType(int mainPathStyleType) {
        this.mainPathStyleType = mainPathStyleType;
    }

    @Override
    public String toString() {
        return "RouteDrawStyles{" +
                "mIsOffLine=" + mIsOffLine +
                ", mIsNavi=" + mIsNavi +
                ", mRouteMapMode=" + mRouteMapMode +
                ", mRouteScene=" + mRouteScene +
                ", mIsMultipleMode=" + mIsMultipleMode +
                ", mainPathStyleType=" + mainPathStyleType +
                '}';
    }
}
