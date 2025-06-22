package com.sgm.navi.scene.ui.navi;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;

public class ChargeTipEntity {
    private String mTitle;
    private String mSubTitle;
    private String mAction;
    @SceneNaviChargeBtnType.Type
    private int mType;
    private String mTtsContent;
    private RouteAlterChargeStationInfo mRouteAlterChargeStationInfo;

    public String getTitle() {
        return mTitle;
    }

    public void setTitle(final String title) {
        this.mTitle = title;
    }

    public String getSubTitle() {
        return mSubTitle;
    }

    public void setSubTitle(final String subTitle) {
        this.mSubTitle = subTitle;
    }

    public String getAction() {
        return mAction;
    }

    public void setAction(final String action) {
        this.mAction = action;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public String getTtsContent() {
        return mTtsContent;
    }

    public void setTtsContent(final String ttsContent) {
        this.mTtsContent = ttsContent;
    }

    public RouteAlterChargeStationInfo getRouteAlterChargeStationInfo() {
        return mRouteAlterChargeStationInfo;
    }

    public void setRouteAlterChargeStationInfo(RouteAlterChargeStationInfo routeAlterChargeStationInfo) {
        this.mRouteAlterChargeStationInfo = routeAlterChargeStationInfo;
    }

    @NonNull
    @Override
    public String toString() {
        return "ChargeTipEntity{" +
                "mTitle='" + mTitle + '\'' +
                ", mSubTitle='" + mSubTitle + '\'' +
                ", mAction='" + mAction + '\'' +
                ", mType=" + mType +
                ", mTtsContent='" + mTtsContent + '\'' +
                ", mRouteAlterChargeStationInfo='" + mRouteAlterChargeStationInfo + '\'' +
                '}';
    }
}
