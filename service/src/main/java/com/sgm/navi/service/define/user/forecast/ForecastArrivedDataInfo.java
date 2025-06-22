package com.sgm.navi.service.define.user.forecast;

import com.sgm.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class ForecastArrivedDataInfo {

    private int mLevel;
    private String mAdCode;
    private String mUserId;
    private GeoPoint mUserLoc;

    private OftenArrivedItemInfo mHome;
    private OftenArrivedItemInfo mCompany;
    private ArrayList<OftenArrivedItemInfo> mOthers;

    public int getLevel() {
        return mLevel;
    }

    public void setLevel(final int level) {
        this.mLevel = level;
    }

    public String getAdCode() {
        return mAdCode;
    }

    public void setAdCode(final String adCode) {
        this.mAdCode = adCode;
    }

    public String getUserId() {
        return mUserId;
    }

    public void setUserId(final String userId) {
        this.mUserId = userId;
    }

    public GeoPoint getUserLoc() {
        return mUserLoc;
    }

    public void setUserLoc(final GeoPoint userLoc) {
        this.mUserLoc = userLoc;
    }

    public OftenArrivedItemInfo getHome() {
        return mHome;
    }

    public void setHome(final OftenArrivedItemInfo home) {
        this.mHome = home;
    }

    public OftenArrivedItemInfo getCompany() {
        return mCompany;
    }

    public void setCompany(final OftenArrivedItemInfo company) {
        this.mCompany = company;
    }

    public ArrayList<OftenArrivedItemInfo> getOthers() {
        return mOthers;
    }

    public void setOthers(final ArrayList<OftenArrivedItemInfo> others) {
        this.mOthers = others;
    }
}
