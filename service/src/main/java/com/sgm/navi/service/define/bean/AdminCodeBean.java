package com.sgm.navi.service.define.bean;

import androidx.annotation.NonNull;

public class AdminCodeBean {
    private int mEuRegionCode = 156;
    private int mNCityAdCode = 0;
    private int mNAdCode = 0;

    public AdminCodeBean() {
    }

    public AdminCodeBean(int euRegionCode, int nCityAdCode, int nAdCode) {
        this.mEuRegionCode = euRegionCode;
        this.mNCityAdCode = nCityAdCode;
        this.mNAdCode = nAdCode;
    }

    public int getEuRegionCode() {
        return mEuRegionCode;
    }

    public void setEuRegionCode(int euRegionCode) {
        this.mEuRegionCode = euRegionCode;
    }

    public int getnCityAdCode() {
        return mNCityAdCode;
    }

    public void setnCityAdCode(int nCityAdCode) {
        this.mNCityAdCode = nCityAdCode;
    }

    public int getnAdCode() {
        return mNAdCode;
    }

    public void setnAdCode(int nAdCode) {
        this.mNAdCode = nAdCode;
    }

    @NonNull
    @Override
    public String toString() {
        return "AdminCodeBean{" +
                "euRegionCode=" + mEuRegionCode +
                ", nCityAdCode=" + mNCityAdCode +
                ", nAdCode=" + mNAdCode +
                '}';
    }
}
