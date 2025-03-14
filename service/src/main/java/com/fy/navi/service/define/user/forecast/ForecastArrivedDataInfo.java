package com.fy.navi.service.define.user.forecast;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class ForecastArrivedDataInfo {

    public int nLevel;
    public String adCode;
    public String userId;
    public GeoPoint userLoc;

    public OftenArrivedItemInfo home;
    public OftenArrivedItemInfo company;
    public ArrayList<OftenArrivedItemInfo> others;

    public int getnLevel() {
        return nLevel;
    }

    public void setnLevel(int nLevel) {
        this.nLevel = nLevel;
    }

    public String getAdCode() {
        return adCode;
    }

    public void setAdCode(String adCode) {
        this.adCode = adCode;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public GeoPoint getUserLoc() {
        return userLoc;
    }

    public void setUserLoc(GeoPoint userLoc) {
        this.userLoc = userLoc;
    }

    public OftenArrivedItemInfo getHome() {
        return home;
    }

    public void setHome(OftenArrivedItemInfo home) {
        this.home = home;
    }

    public OftenArrivedItemInfo getCompany() {
        return company;
    }

    public void setCompany(OftenArrivedItemInfo company) {
        this.company = company;
    }

    public ArrayList<OftenArrivedItemInfo> getOthers() {
        return others;
    }

    public void setOthers(ArrayList<OftenArrivedItemInfo> others) {
        this.others = others;
    }
}
