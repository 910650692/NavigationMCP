package com.fy.navi.service.define.bean;


import androidx.annotation.NonNull;

public class AreaExtraInfoBean {
    private AdminCodeBean mStAdCode;
    private AdMapPointBean mStCenterPoint;
    private String mProvName;

    private String mCityName;
    private String mTownName;

    public AdminCodeBean getStAdCode() {
        return mStAdCode;
    }

    public void setStAdCode(AdminCodeBean stAdCode) {
        this.mStAdCode = stAdCode;
    }

    public AdMapPointBean getStCenterPoint() {
        return mStCenterPoint;
    }

    public void setStCenterPoint(AdMapPointBean stCenterPoint) {
        this.mStCenterPoint = stCenterPoint;
    }

    public String getProvName() {
        return mProvName;
    }

    public void setProvName(String provName) {
        this.mProvName = provName;
    }

    public String getCityName() {
        return mCityName;
    }

    public void setCityName(String cityName) {
        this.mCityName = cityName;
    }

    public String getTownName() {
        return mTownName;
    }

    public void setTownName(String townName) {
        this.mTownName = townName;
    }

    @NonNull
    @Override
    public String toString() {
        return "AreaExtraInfoBean{" +
                "stAdCode=" + mStAdCode +
                ", stCenterPoint=" + mStCenterPoint +
                ", provName='" + mProvName + '\'' +
                ", cityName='" + mCityName + '\'' +
                ", townName='" + mTownName + '\'' +
                '}';
    }
}
