package com.fy.navi.service.define.mapdata;

public class MergedStatusBean {

    private boolean mMergedSate;
    private int mAdCode;
    private String mCityName;
    private String mPackageType;
    private String mErrType;
    private String mUpdateType;
    private String mUrl;
    private String mErrTypeDetill;
    private String mCostMergeTime;

    /**
     * 设置参数
     * @return 返回 boolean型数据
     */
    public boolean isMergedSate() {
        return mMergedSate;
    }

    /**
     * 获取参数
     * @param mergedSate
     */
    public void setMergedSate(final boolean mergedSate) {
        this.mMergedSate = mergedSate;
    }

    public int getAdcode() {
        return mAdCode;
    }

    public void setAdcode(final int adcode) {
        this.mAdCode = adcode;
    }

    public String getCityName() {
        return mCityName;
    }

    public void setCityName(final String cityName) {
        this.mCityName = cityName;
    }

    public String getPackageType() {
        return mPackageType;
    }

    public void setPackageType(final String packageType) {
        this.mPackageType = packageType;
    }

    public String getErrType() {
        return mErrType;
    }

    public void setErrType(final String errType) {
        this.mErrType = errType;
    }

    public String getUpdateType() {
        return mUpdateType;
    }

    public void setUpdateType(final String updateType) {
        this.mUpdateType = updateType;
    }

    public String getUrl() {
        return mUrl;
    }

    public void setUrl(final String url) {
        this.mUrl = url;
    }

    public String getErrTypeDetill() {
        return mErrTypeDetill;
    }

    public void setErrTypeDetill(final String errTypeDetill) {
        this.mErrTypeDetill = errTypeDetill;
    }

    public String getCostMergeTime() {
        return mCostMergeTime;
    }

    public void setCostMergeTime(final String costMergeTime) {
        this.mCostMergeTime = costMergeTime;
    }
}
