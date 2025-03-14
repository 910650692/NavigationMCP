package com.fy.navi.service.define.user.usertrack;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/27
 */
public class HistoryPoiItemBean {
    public String itemId;
    public String poiId;
    public String typeCode;
    public String name;
    public String address;
    public GeoPoint poiLoc;
    public GeoPoint navLoc;
    public String parent;
    public int childType;
    public String towardsAngle;
    public String floorNo;
    public int endPoiExtension;
    public long updateTime;

    public int cityCode;
    public String cityName;

    public ArrayList<GeoPoint> entranceList;


    public String getItemId() {
        return itemId;
    }

    public void setItemId(String itemId) {
        this.itemId = itemId;
    }

    public String getPoiId() {
        return poiId;
    }

    public void setPoiId(String poiId) {
        this.poiId = poiId;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public GeoPoint getPoiLoc() {
        return poiLoc;
    }

    public void setPoiLoc(GeoPoint poiLoc) {
        this.poiLoc = poiLoc;
    }

    public GeoPoint getNavLoc() {
        return navLoc;
    }

    public void setNavLoc(GeoPoint navLoc) {
        this.navLoc = navLoc;
    }

    public String getParent() {
        return parent;
    }

    public void setParent(String parent) {
        this.parent = parent;
    }

    public int getChildType() {
        return childType;
    }

    public void setChildType(int childType) {
        this.childType = childType;
    }

    public String getTowardsAngle() {
        return towardsAngle;
    }

    public void setTowardsAngle(String towardsAngle) {
        this.towardsAngle = towardsAngle;
    }

    public String getFloorNo() {
        return floorNo;
    }

    public void setFloorNo(String floorNo) {
        this.floorNo = floorNo;
    }

    public int getEndPoiExtension() {
        return endPoiExtension;
    }

    public void setEndPoiExtension(int endPoiExtension) {
        this.endPoiExtension = endPoiExtension;
    }

    public long getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(long updateTime) {
        this.updateTime = updateTime;
    }

    public int getCityCode() {
        return cityCode;
    }

    public void setCityCode(int cityCode) {
        this.cityCode = cityCode;
    }

    public String getCityName() {
        return cityName;
    }

    public void setCityName(String cityName) {
        this.cityName = cityName;
    }

    public ArrayList<GeoPoint> getEntranceList() {
        return entranceList;
    }

    public void setEntranceList(ArrayList<GeoPoint> entranceList) {
        this.entranceList = entranceList;
    }
}
