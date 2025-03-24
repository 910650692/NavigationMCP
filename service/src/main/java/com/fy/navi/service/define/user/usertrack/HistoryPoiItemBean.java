package com.fy.navi.service.define.user.usertrack;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;


public class HistoryPoiItemBean {

    private String mItemId;
    private String mPoiId;
    private String mTypeCode;
    private String mName;
    private String mAddress;
    private GeoPoint mPoiLoc;
    private GeoPoint mNavLoc;
    private String mParent;
    private int mChildType;
    private String mTowardsAngle;
    private String mFloorNo;
    private int mEndPoiExtension;
    private long mUpdateTime;

    private int mCityCode;
    private String mCityName;

    private ArrayList<GeoPoint> mEntranceList;


    public String getItemId() {
        return mItemId;
    }

    public void setItemId(final String itemId) {
        this.mItemId = itemId;
    }

    public String getPoiId() {
        return mPoiId;
    }

    public void setPoiId(final String poiId) {
        this.mPoiId = poiId;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    public void setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        this.mName = name;
    }

    public String getAddress() {
        return mAddress;
    }

    public void setAddress(final String address) {
        this.mAddress = address;
    }

    public GeoPoint getPoiLoc() {
        return mPoiLoc;
    }

    public void setPoiLoc(final GeoPoint poiLoc) {
        this.mPoiLoc = poiLoc;
    }

    public GeoPoint getNavLoc() {
        return mNavLoc;
    }

    public void setNavLoc(final GeoPoint navLoc) {
        this.mNavLoc = navLoc;
    }

    public String getParent() {
        return mParent;
    }

    public void setParent(final String parent) {
        this.mParent = parent;
    }

    public int getChildType() {
        return mChildType;
    }

    public void setChildType(final int childType) {
        this.mChildType = childType;
    }

    public String getTowardsAngle() {
        return mTowardsAngle;
    }

    public void setTowardsAngle(final String towardsAngle) {
        this.mTowardsAngle = towardsAngle;
    }

    public String getFloorNo() {
        return mFloorNo;
    }

    public void setFloorNo(final String floorNo) {
        this.mFloorNo = floorNo;
    }

    public int getEndPoiExtension() {
        return mEndPoiExtension;
    }

    public void setEndPoiExtension(final int endPoiExtension) {
        this.mEndPoiExtension = endPoiExtension;
    }

    public long getUpdateTime() {
        return mUpdateTime;
    }

    public void setUpdateTime(final long updateTime) {
        this.mUpdateTime = updateTime;
    }

    public int getCityCode() {
        return mCityCode;
    }

    public void setCityCode(final int cityCode) {
        this.mCityCode = cityCode;
    }

    public String getCityName() {
        return mCityName;
    }

    public void setCityName(final String cityName) {
        this.mCityName = cityName;
    }

    public ArrayList<GeoPoint> getEntranceList() {
        return mEntranceList;
    }

    public void setEntranceList(final ArrayList<GeoPoint> entranceList) {
        this.mEntranceList = entranceList;
    }
}
