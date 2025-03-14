package com.fy.navi.service.define.mapdata;

/**
 * @Description
 * @Author fh
 * @date 2024/12/1
 */
public class MergedStatusBean {

    public boolean bMergedSate;
    public int adcode;
    public String cityName;
    public String packageType;
    public String errType;
    public String updateType;
    public String url;
    public String errTypeDetill;
    public String costMergeTime;

    public boolean isbMergedSate() {
        return bMergedSate;
    }

    public void setbMergedSate(boolean bMergedSate) {
        this.bMergedSate = bMergedSate;
    }

    public int getAdcode() {
        return adcode;
    }

    public void setAdcode(int adcode) {
        this.adcode = adcode;
    }

    public String getCityName() {
        return cityName;
    }

    public void setCityName(String cityName) {
        this.cityName = cityName;
    }

    public String getPackageType() {
        return packageType;
    }

    public void setPackageType(String packageType) {
        this.packageType = packageType;
    }

    public String getErrType() {
        return errType;
    }

    public void setErrType(String errType) {
        this.errType = errType;
    }

    public String getUpdateType() {
        return updateType;
    }

    public void setUpdateType(String updateType) {
        this.updateType = updateType;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getErrTypeDetill() {
        return errTypeDetill;
    }

    public void setErrTypeDetill(String errTypeDetill) {
        this.errTypeDetill = errTypeDetill;
    }

    public String getCostMergeTime() {
        return costMergeTime;
    }

    public void setCostMergeTime(String costMergeTime) {
        this.costMergeTime = costMergeTime;
    }
}
